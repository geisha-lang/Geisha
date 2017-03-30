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

import Text.Parsec.Pos

import Geisha.AST
import Geisha.Error
import Geisha.TypeInfer.Env
-- type Name = String

typeInt, typeBool, typeFloat, typeStr :: Loc -> GType
typeInt loc   = TCon loc "Int"
typeBool loc  = TCon loc "Bool"
typeFloat loc = TCon loc "Float"
typeStr loc   = TCon loc "Str"

arrow, product :: Loc -> GType -> GType -> GType
arrow = TArr
product = TProd

productMany :: Loc -> [GType] -> GType
productMany loc [] = Void loc
productMany loc ts = setLoc (foldl1 (TProd loc) ts) loc


type Subst = M.Map Name GType

emptySubst :: Subst
emptySubst = M.empty

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set Name

instance Substitutable GType where
  apply _ v@(Void _)        = v
  apply _ (TCon l a)        = TCon l a
  apply s t@(TVar _ a)      = M.findWithDefault t a s
  apply s (TArr pos t1 t2)  = arrow pos (apply s t1) $ apply s t2
  apply s (TProd pos t1 t2) = product pos (apply s t1) $ apply s t2

  ftv (Void _) = S.empty
  ftv (TCon _ _) = S.empty
  ftv (TVar _ a) = S.singleton a
  ftv (TArr _ t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TProd _ t1 t2) = ftv t1 `S.union` ftv t2

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


type Solve = Except TypeError

runSolve :: [Constraint] -> ThrowsTypeError Subst
runSolve cs = runExcept $ solver (emptySubst, cs)

runInfer :: TypeEnv -> Infer a -> ThrowsTypeError (a, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer


closeOver :: GType -> Infer Scheme
-- closeOver t@Forall{} = return t
closeOver t = normalize $ generalize emptyEnv t

-- | make type var names like `a0`, `a1` normalize to `a`, `b`
normalize :: Scheme -> Infer Scheme
normalize (Forall _ body) = Forall (map snd ord) <$> normtype body
  where ord = zip (nub $ fv body) letters    -- generated type names

        fv (Void _)        = []
        fv (TCon _ _)    = []
        fv (TVar _ a)    = [a]
        fv (TArr _ a b)  = fv a ++ fv b
        fv (TProd _ a b) = fv a ++ fv b

        normtype :: GType -> Infer GType
        normtype t@Void{}      = return t
        normtype (TArr l a b)  = liftA2 (TArr l) (normtype a) (normtype b)
        normtype (TProd l a b) = liftA2 (TProd l) (normtype a) (normtype b)
        normtype c@TCon{}      = return c
        normtype (TVar l a)    = case Prelude.lookup a ord of
          Just x -> return $ TVar l x
          Nothing -> throwError $ Reserved "type variable not in signature"
normalize ty = return ty

generalize :: TypeEnv -> GType -> Scheme
generalize env t = Forall as t
  where as = S.toList $ ftv t S.\\ ftv env



letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Loc -> Infer GType
fresh loc = do
  s <- get
  put $ s { count = count s + 1 }
  return . TVar loc $ letters !! count s


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

instantiate :: Scheme -> Infer GType
instantiate (Forall as t) = do
  as' <- mapM (const . fresh $ getLoc t) as
  let s = M.fromList $ zip as as'
  return $ apply s t
-- instantiate t = return t

lookupEnv :: Name -> Infer GType
lookupEnv x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
    Nothing -> throwError $ NotInScope x
    Just s  -> instantiate s

inferTop :: [Syntax] -> Infer [Syntax]
inferTop ds = do
  tvs <- mapM (fresh . getLoc . syntaxType) ds
  env <- ask
  let ns = map declName ds
      tvs' = map (generalize env) tvs
      pairs = zip ns tvs'
  inEnv' pairs $ inferDecls ds
  where declName (Decl _ (Define n _)) = n

inferDecls :: [Syntax] -> Infer [Syntax]
inferDecls [] = return []
inferDecls (Decl _ (Define name e) : ds) = do
  (form, ty) <- inferExpr e
  scm <- closeOver ty
  fs <- inEnv (name, scm) $ inferDecls ds
  return $ form : fs


inferExpr :: Syntax -> Infer (Syntax, GType)
inferExpr e = do
  env <- ask
  case runInfer env $ infer e of
    Right ((Expr anno ex, ty), cs) -> case runSolve cs of
      Left err    -> throwError err
      Right subst -> do
        scm <- closeOver $ apply subst ty
        return (Expr (anno { _type = scm }) ex, ty)
    Left err  -> throwError err

infer :: Syntax -> Infer (Syntax, GType)
infer (Expr anno@(Annotation pos ty) ex) =
  let withType :: GType -> Infer (Syntax, GType)
      withType ty = do
        cty <- closeOver ty
        return (Expr (anno { _type = cty }) ex, ty)
      
      typeNode :: Expr -> GType -> Infer (Syntax, GType)
      typeNode exp ty = do
        cty <- closeOver ty
        return (Expr (anno { _type = cty }) exp, ty)
  
  in case ex of
    Lit lit -> withType $ ty pos
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
          nargs = map varName xs
      tvs <- mapM (const $ fresh NoLoc) xs
      let targs = productMany pos tvs
          scms  = map forall tvs
      (body, tbody) <- inEnv' (zip nargs scms) (infer e)
      typeNode (Function $ Lambda xs body) $ arrow pos targs tbody

    
    Apply f args -> do
      (f, tf) <- infer f
      (args, targs) <- unzip <$> mapM infer args
      -- targ <- productMany <$> mapM infer args
      let targ = productMany pos targs
      tv <- fresh NoLoc
      uni tf $ arrow pos targ tv
      typeNode (Apply f args) tv


    Let (Expr annov@(Annotation vpos vt) letv@(Var x)) e1 e2 -> do
      env <- ask
      (e1, te1) <- infer e1
      -- let te1 = _type e1
      let sc = generalize env te1
      (lete, tlete) <- inEnv (x, sc) (infer e2)
      ctlete <- closeOver tlete
      return (Expr (anno { _type = ctlete }) $ Let (Expr (annov { _type = sc }) letv) e1 lete, tlete)
    
    If cond tr fl -> do
      (cond, tcond) <- infer cond
      (tr, ttr) <- infer tr
      (fl, tfl) <- infer fl
      uni tcond (typeBool $ getLoc tcond)
      uni ttr tfl
      typeNode (If cond tr fl) ttr


    exp@(Block []) -> throwError $ BadTypeForm "Zero length block" exp
    Block es -> do
      (es, tes) <- unzip <$> mapM infer es
      typeNode (Block es) $ last tes

    bad -> throwError . Reserved $ "Not yet implemented: " ++ show bad



occursCheck :: Substitutable a => Name -> a -> Bool
occursCheck a t = a `S.member` ftv t

bind :: Name -> GType -> Solve Subst
bind a TVar{} = return emptySubst
bind a t
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ M.singleton a t


eqNoLoc l r = setLoc l NoLoc == setLoc r NoLoc

unifies :: GType -> GType -> Solve Subst
unifies t1 t2 | eqNoLoc t1 t2           = return emptySubst
unifies (TVar _ v) t                    = v `bind` t
unifies t (TVar _ v)                    = v `bind` t
unifies (TArr _ t1 t2) (TArr _ t3 t4)   = unifyMany [t1, t2] [t3, t4]
unifies (TProd _ t1 t2) (TProd _ t3 t4) = unifyMany [t1, t2] [t3, t4]

-- | 这里大概，可以直接不管 Forall 里面？
-- | 然而我觉得可能还是要把 Forall 和一般的单态分开，只在 AST 标记里放一起
-- unifies (Forall _ t1) t2            = unifies t1 t2
-- unifies t1 (Forall _ t2)            = unifies t1 t2
unifies t1 t2                       = throwError $ Mismatch t1 t2

unifyMany :: [GType] -> [GType] -> Solve Subst
unifyMany [] []                 = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

composeMany :: [Subst] -> Subst
composeMany = foldl1 compose

solver :: Unifier -> Solve Subst
solver (su, cs) = case cs of
  [] -> return su
  ((t1, t2) : cs0) -> do
    su1 <- unifies t1 t2
    solver (su1 `compose` su, apply su1 cs0)

