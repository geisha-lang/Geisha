{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Geisha.TypeInfer where

import Prelude hiding (product)

import qualified Data.HashMap as M
import qualified Data.Set as S

import Data.List (nub, last)

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Geisha.AST
import Geisha.Error

-- type Name = String

typeInt, typeBool, typeFloat, typeStr :: GType
typeInt = TCon "Int"
typeBool = TCon "Bool"
typeFloat = TCon "Float"
typeStr = TCon "Str"

arrow, product :: GType -> GType -> GType
arrow = TArr
product = TProd

productMany :: [GType] -> GType
productMany [] = Void
productMany ts = foldl1 product ts

newtype TypeEnv = TypeEnv (M.Map Name Scheme)
                deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv m) = unlines $ map (\(n, t) -> n ++ " :: " ++ show t) $ M.toList m

emptyEnv = TypeEnv M.empty
envList = TypeEnv . M.fromList

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (n, s) = TypeEnv $ M.insert n s env

type Subst = M.Map Name GType

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

composeAll :: [Subst] -> Subst
composeAll = foldl1 compose

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set Name

instance Substitutable GType where
  apply _ Void = Void
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = M.findWithDefault t a s
  apply s (TArr t1 t2) = apply s t1 `TArr` apply s t2
  apply s (TProd t1 t2) = apply s t1 `TProd` apply s t2

  ftv Void = S.empty
  ftv (TCon _) = S.empty
  ftv (TVar a) = S.singleton a
  ftv (TArr t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TProd t1 t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete s as
  ftv (Forall as t) = ftv t S.\\ S.fromList as

instance Substitutable Constraint where
   apply s (t1, t2) = (apply s t1, apply s t2)
   ftv (t1, t2) = ftv t1 `S.union` ftv t2

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv e) = TypeEnv $ M.map (apply s) e
  ftv (TypeEnv e)     = ftv $ M.elems e

newtype InferState = InferState { count :: Int }
initInfer = InferState { count = 0 }
-- type Infer a = ExceptT TypeError (State Unique) a
type Infer = RWST TypeEnv [Constraint] InferState (Except TypeError)

type Constraint = (GType, GType)
type Unifier = (Subst, [Constraint])

-- type Solve = StateT Unifier (Except TypeError)
type Solve = Except TypeError

runSolve :: [Constraint] -> Either TypeError Subst
runSolve cs = runExcept $ solver (emptySubst, cs)

runInfer :: TypeEnv -> Infer a -> Either TypeError (a, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

-- newtype Unique = Unique { count :: Int }
-- initUnique = Unique 0

-- runInfer :: Infer (Subst, GType) -> Either TypeError Scheme
-- runInfer m = case evalState (runExceptT m) initUnique of
--   Right (sub, res) -> Right $ closeOver res
--   Left err         -> throwError err

closeOver :: GType -> Scheme
closeOver = normalize . generalize emptyEnv

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where ord = zip (nub $ fv body) letters
        fv Void          = []
        fv (TCon _)      = []
        fv (TVar a)      = [a]
        fv (TArr a b) = fv a ++ fv b
        fv (TProd a b) = fv a ++ fv b

        normtype Void       = Void
        normtype (TArr a b) = normtype a `TArr` normtype b
        normtype (TProd a b) = normtype a `TProd` normtype b
        normtype (TCon a)   = TCon a
        normtype (TVar a)   =
          case Prelude.lookup a ord of
            Just x -> TVar x
            Nothing -> error "type variable not in signature"


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer GType
fresh = do
  s <- get
  put $ s { count = count s + 1 }
  return . TVar $ letters !! count s

occursCheck :: Substitutable a => Name -> a -> Bool
occursCheck a t = a `S.member` ftv t

bind :: Name -> GType -> Solve Subst
bind a t
  | t == TVar a     = return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ M.singleton a t

-- unify :: GType -> GType -> Infer Subst
-- unify (TComp n l r) (TComp n' l' r') 
--   | n == n' = do
--       s1 <- unify l l'
--       s2 <- unify (apply s1 r) (apply s1 r')
--       return $ s2 `compose` s1

-- unify (TVar a) t = bind a t
-- unify t (TVar a) = bind a t
-- -- unify (TCon a) (TCon b)
-- --   | a == b = return emptySubst
-- unify t1 t2
--   | t1 == t2  = return emptySubst
--   | otherwise = throwError $ Mismatch t1 t2

uni :: GType -> GType -> Infer ()
uni tl tr = tell [(tl, tr)]

inEnv :: (Name, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

inEnv' :: [(Name, Scheme)] -> Infer a -> Infer a
inEnv' ps m = do
  let extender e (x, sc) = remove e x `extend` (x, sc)
  local (\x -> foldl extender x ps) m

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) v = TypeEnv $ M.delete v env

instantiate :: Scheme -> Infer GType
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = M.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> GType -> Scheme
generalize env t = Forall as t
  where as = S.toList $ ftv t S.\\ ftv env

-- lookupEnv :: TypeEnv -> Name -> Infer (Subst, GType)
-- lookupEnv (TypeEnv e) x = case x `M.lookup` e of
--   Just t -> do
--     t' <- instantiate t
--     return (emptySubst, t')
--   _      -> throwError $ NotInScope x

lookupEnv :: Name -> Infer GType
lookupEnv x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
    Nothing -> throwError $ NotInScope x
    Just s  -> instantiate s

inferTop :: [AST] -> Infer TypeEnv
inferTop ds = do
  tvs <- mapM (const fresh) ds
  env <- ask
  let ns = map declName ds
      tvs' = map (generalize env) tvs
      pairs = zip ns tvs'
  inEnv' pairs $ inferDecls ds
  -- let env' = foldl extend env pairs
  -- case inferDecls env' ds of
  --   Right e  -> return e
  --   Left err -> throwError err
  where declName (Decl _ _ (Define n _)) = n

inferDecls :: [AST] -> Infer TypeEnv
inferDecls [] = ask
inferDecls (Decl pos ty (Define name e) : ds) = do
  scm <- inferExpr e
  inEnv (name, scm) $ inferDecls ds
  -- inferDecls (env `extend` (name, scm)) ds
-- case inferExpr env e of
--   Left err  -> throwError err
--   Right scm -> inferDecls (env `extend` (name, scm)) ds

inferExpr :: AST -> Infer Scheme
-- inferExpr env [] = return env
inferExpr e = do
  -- ty <- infer e
  -- case runSolve cs of
  --   Left err    -> throwError err
  --   Right subst -> return . closeOver $ apply subst ty
  env <- ask
  case runInfer env $ infer e of
    Right (ty, cs) -> case runSolve cs of
      Left err    -> throwError err
      Right subst -> return . closeOver $ apply subst ty
      -- inferDecl (extend env (name, scm)) es
    Left err  -> throwError err

infer :: AST -> Infer GType
infer (Expr pos ty ex) = case ex of
  Lit lit -> return $ case lit of
    LInt{} -> typeInt
    LStr{} -> typeStr
    LBool{} -> typeBool
    LFloat{} -> typeFloat

  Var x -> lookupEnv x

  -- Function (Lambda [] e) -> arrow Void <$> infer e 
  Function (Lambda xs e) -> do
    let forall = Forall []
        nargs = map (\(Expr _ _ (Var n)) -> n) xs
    tvs <- mapM (const fresh) xs
    let targs = productMany tvs
        scms  = map forall tvs
    t <- inEnv' (zip nargs scms) (infer e)
    return $ targs `arrow` t
  
  Apply f args -> do
    tf <- infer f
    targ <- productMany <$> mapM infer args
    tv <- fresh
    uni tf $ targ `arrow` tv
    return tv

  Let (Expr _ _ (Var x)) e1 e2 -> do
    env <- ask
    t1 <- infer e1
    let sc = generalize env t1
    inEnv (x, sc) (infer e2)
  
  If cond tr fl -> do
    t1 <- infer cond
    t2 <- infer tr
    t3 <- infer fl
    uni t1 typeBool
    uni t2 t3
    return t2
  exp@(Block es) -> reducer es
        where reducer []     = throwError $ BadTypeForm "Zero length block: " exp
              reducer [e]    = infer e
              reducer (e:es) = infer e >> reducer es
  bad -> throwError $ Reserved "Not yet implemented: " bad

unifies :: GType -> GType -> Solve Subst
unifies t1 t2 | t1 == t2 = return emptySubst
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies (TProd t1 t2) (TProd t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ Mismatch t1 t2

unifyMany :: [GType] -> [GType] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  -- (su1, cs1) <- unifies t1 t2
  -- (su2, cs2) <- unifyMany (apply su1 ts1) (apply su1 ts2)
  -- return (su2 `compose` su1, cs1 ++ cs2)
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2) : cs0) -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

-- infer :: TypeEnv -> AST -> Infer (Subst, GType)
-- infer env (Expr pos ty ex) = case ex of
--   Lit lit -> let
--     constType ty = return (emptySubst, ty)
--     in constType $ case lit of
--       LInt{} -> typeInt
--       LStr{} -> typeStr
--       LBool{} -> typeBool
--       LFloat{} -> typeFloat
--   Var x -> lookupEnv env x
--   Function (Lambda [] e) -> do
--     (_, ty) <- infer env e
--     return (emptySubst, arrow Void ty)
--   Function (Lambda x e) -> do
--     tvs <- mapM (const fresh) x
--     let tvargs = map (Forall []) tvs
--         nargs = map (\(Expr _ _ (Var n)) -> n) x
--         env' = foldl extend env $ zip nargs tvargs

--     (s', t') <- infer env' e
--     let targs = productMany $ apply s' tvs
--     return (s', targs `arrow` t')
  
--   Apply e [] -> do
--     (s, t) <- infer env e
--     tv <- fresh
--     s <- unify t $ Void `arrow` tv
--     return (s, apply s tv)
--   Apply e1 e2 -> do
--     tv <- fresh
--     (s1, t1) <- infer env e1
--     (s2s, t2s) <- fmap unzip $ mapM (infer $ apply s1 env) e2
--     -- 这里应该逐一应用参数合一后的变换
--     -- 或者直接比两个 Production ?
--     let s2 = composeAll s2s
--     let t2 = productMany t2s
--     s3 <- unify (apply s2 t1) $ t2 `arrow` tv
--     return (s3 `compose` s2 `compose` s1, apply s3 tv)

--   Let (Expr _ _ (Var x)) e1 e2 -> do
--     (s1, t1) <- infer env e1
--     let env' = apply s1 env
--         t'   = generalize env' t1
--     (s2, t2) <- infer (env' `extend` (x, t')) e2
--     return (s1 `compose` s2, t2)

--   If cond tr fl -> do
--     (s1, t1) <- infer env cond
--     let env1 = apply s1 env
--     (s2, t2) <- infer env1 tr
--     (s3, t3) <- infer env1 fl
--     s4 <- unify t1 typeBool
--     s5 <- unify t2 t3
--     return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  
--   Block b -> reducer env b emptySubst
--     -- 至少要保证 Block 中所有的 expr 都能与其它的成功合一
--     where reducer env [e] s = do
--             (s', t) <- infer env e
--             return (s `compose` s', t)
--           reducer env (e:es) s = do
--             (s', t) <- infer env e
--             let env' = apply s env
--             reducer env' es $ s `compose` s'


--   bad -> throwError $ Reserved "Not yet implemented: " bad

