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
typeInt   = TCon "Int"
typeBool  = TCon "Bool"
typeFloat = TCon "Float"
typeStr   = TCon "Str"

arrow, product :: GType -> GType -> GType
arrow   = TArr
product = TProd

productMany :: [GType] -> GType
productMany [] = Void
productMany ts = foldl1 product ts

newtype TypeEnv = TypeEnv (M.Map Name GType)
                deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv m) = unlines $ map (\(n, t) -> n ++ " :: " ++ show t) $ M.toList m

emptyEnv = TypeEnv M.empty
envList = TypeEnv . M.fromList

extend :: TypeEnv -> (Name, GType) -> TypeEnv
extend (TypeEnv env) (n, s) = TypeEnv $ M.insert n s env

type Subst = M.Map Name GType

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

composeMany :: [Subst] -> Subst
composeMany = foldl1 compose

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set Name

instance Substitutable GType where
  apply _ Void = Void
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = M.findWithDefault t a s
  apply s (TArr t1 t2) = apply s t1 `TArr` apply s t2
  apply s (TProd t1 t2) = apply s t1 `TProd` apply s t2
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete s as

  ftv Void = S.empty
  ftv (TCon _) = S.empty
  ftv (TVar a) = S.singleton a
  ftv (TArr t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TProd t1 t2) = ftv t1 `S.union` ftv t2
  ftv (Forall as t) = ftv t S.\\ S.fromList as

-- instance Substitutable GType where
--   apply s (Forall as t) = Forall as $ apply s' t
--     where s' = foldr M.delete s as
--   ftv (Forall as t) = ftv t S.\\ S.fromList as

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


type Solve = Except TypeError

runSolve :: [Constraint] -> ThrowsTypeError Subst
runSolve cs = runExcept $ solver (emptySubst, cs)

runInfer :: TypeEnv -> Infer a -> ThrowsTypeError (a, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer


closeOver :: GType -> Infer GType
closeOver t@Forall{} = return t
closeOver t          = normalize $ generalize emptyEnv t

-- | make type var names like `a0`, `a1` normalize to `a`, `b`
normalize :: GType -> Infer GType
normalize (Forall _ body) = Forall (map snd ord) <$> normtype body
  where ord = zip (nub $ fv body) letters    -- generated type names

        fv Void        = []
        fv (TCon _)    = []
        fv (TVar a)    = [a]
        fv (TArr a b)  = fv a ++ fv b
        fv (TProd a b) = fv a ++ fv b

        normtype :: GType -> Infer GType
        normtype Void        = return Void
        normtype (TArr a b)  = liftA2 TArr (normtype a) (normtype b)
        normtype (TProd a b) = liftA2 TProd (normtype a) (normtype b)
        normtype (TCon a)    = return $ TCon a
        normtype (TVar a)    = case Prelude.lookup a ord of
          Just x -> return $ TVar x
          Nothing -> throwError $ Reserved "type variable not in signature"
normalize ty = return ty

generalize :: TypeEnv -> GType -> GType
generalize _ t@Forall{} = t
generalize env t = case as of
  [] -> t
  _  -> Forall as t
  where as = S.toList $ ftv t S.\\ ftv env



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


uni :: GType -> GType -> Infer ()
uni tl tr = tell [(tl, tr)]

inEnv :: (Name, GType) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = remove e x `extend` (x, sc)
  local scope m

inEnv' :: [(Name, GType)] -> Infer a -> Infer a
inEnv' ps m = do
  let extender e (x, sc) = remove e x `extend` (x, sc)
  local (\x -> foldl extender x ps) m

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) v = TypeEnv $ M.delete v env

instantiate :: GType -> Infer GType
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = M.fromList $ zip as as'
  return $ apply s t
instantiate t = return t

lookupEnv :: Name -> Infer GType
lookupEnv x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
    Nothing -> throwError $ NotInScope x
    Just s  -> instantiate s

inferTop :: [Form] -> Infer [Form]
inferTop ds = do
  tvs <- mapM (const fresh) ds
  env <- ask
  let ns = map declName ds
      tvs' = map (generalize env) tvs
      pairs = zip ns tvs'
  inEnv' pairs $ inferDecls ds
  where declName (Decl (ASTNode _ _ (Define n _))) = n

inferDecls :: [Form] -> Infer [Form]
inferDecls [] = return []
inferDecls (Decl (ASTNode pos ty (Define name e)) : ds) = do
  (form, scm) <- inferExpr e
  fs <- inEnv (name, scm) $ inferDecls ds
  return $ form : fs


inferExpr :: Form -> Infer (Form, GType)
inferExpr e = do
  env <- ask
  case runInfer env $ infer e of
    Right ((Expr node, ty), cs) -> case runSolve cs of
      Left err    -> throwError err
      Right subst -> do
        ty <- closeOver $ apply subst ty
        return (Expr $ node { _type = ty }, ty)
    Left err  -> throwError err
{-
infer :: Form -> Infer GType
infer (Expr pos ty ex) = case ex of
  Lit lit -> return $ case lit of
    LInt{} -> typeInt
    LStr{} -> typeStr
    LBool{} -> typeBool
    LFloat{} -> typeFloat

  Var x -> lookupEnv x


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
  bad -> throwError . Reserved $ "Not yet implemented: " ++ show bad
-}


infer :: Form -> Infer (Form, GType)
infer (Expr node@(ASTNode pos ty ex)) =
  let withType :: GType -> Infer (Form, GType)
      withType ty = do
        cty <- closeOver ty
        return (Expr $ node { _type = cty }, ty)
      
      typeNode :: Expr -> GType -> Infer (Form, GType)
      typeNode exp ty = do
        cty <- closeOver ty
        return (Expr $ node {
          _type = cty,
          _form = exp
        }, ty)
  
  in case ex of
    Lit lit -> withType ty
      where ty = case lit of
              LInt{} -> typeInt
              LStr{} -> typeStr
              LBool{} -> typeBool
              LFloat{} -> typeFloat

    Var x -> do
      ty <- lookupEnv x
      withType ty


    Function (Lambda xs e) -> do
      let forall = Forall []
          nargs = map (\(Expr (ASTNode _ _ (Var n))) -> n) xs
      tvs <- mapM (const fresh) xs
      let targs = productMany tvs
          scms  = map forall tvs
      (body, tbody) <- inEnv' (zip nargs scms) (infer e)
      typeNode (Function $ Lambda xs body) $ targs `arrow` tbody
      -- return . Expr $ node {
      --   _type = targs `arrow` _type body,
      --   _form = Function $ Lambda xs body
      -- }
    
    Apply f args -> do
      (f, tf) <- infer f
      (args, targs) <- unzip <$> mapM infer args
      -- targ <- productMany <$> mapM infer args
      let targ = productMany targs
      tv <- fresh
      uni tf $ targ `arrow` tv
      typeNode (Apply f args)  tv
      -- return . Expr $ node {
      --   _type = tv,
      --   _form = Apply f args
      -- }

    Let (Expr letv@(ASTNode _ vt (Var x))) e1 e2 -> do
      env <- ask
      (e1, te1) <- infer e1
      -- let te1 = _type e1
      let sc = generalize env te1
      (lete, tlete) <- inEnv (x, sc) (infer e2)
      ctlete <- closeOver tlete
      return (Expr node {
        _type = ctlete,
        _form = Let (Expr (letv { _type = sc })) e1 lete
      }, tlete)
    
    If cond tr fl -> do
      (cond, tcond) <- infer cond
      (tr, ttr) <- infer tr
      (fl, tfl) <- infer fl
      uni tcond typeBool
      uni ttr tfl
      typeNode (If cond tr fl) ttr
      -- return . Expr $ node {
      --   _type = _type tr,
      --   _form = If cond tr fl
      -- }

    -- exp@(Block es) -> reducer es
    exp@(Block []) -> throwError $ BadTypeForm "Zero length block" exp
    Block es -> do
      (es, tes) <- unzip <$> mapM infer es
      typeNode (Block es) $ last tes
      -- return . Expr $ node {
      --   _type = _type $ last es,
      --   _form = Block es
      -- }
    bad -> throwError . Reserved $ "Not yet implemented: " ++ show bad


unifies :: GType -> GType -> Solve Subst
unifies t1 t2 | t1 == t2            = return emptySubst
unifies (TVar v) t                  = v `bind` t
unifies t (TVar v)                  = v `bind` t
unifies (TArr t1 t2) (TArr t3 t4)   = unifyMany [t1, t2] [t3, t4]
unifies (TProd t1 t2) (TProd t3 t4) = unifyMany [t1, t2] [t3, t4]

-- | 这里大概，可以直接不管 Forall 里面？
-- | 然而我觉得可能还是要把 Forall 和一般的单态分开，只在 AST 标记里放一起
unifies (Forall _ t1) t2            = unifies t1 t2
unifies t1 (Forall _ t2)            = unifies t1 t2
unifies t1 t2                       = throwError $ Mismatch t1 t2

unifyMany :: [GType] -> [GType] -> Solve Subst
unifyMany [] []                 = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
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

