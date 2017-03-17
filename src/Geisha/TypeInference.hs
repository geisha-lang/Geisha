module Geisha.TypeInference where

import qualified Data.HashMap as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Applicative
import Control.Monad.Except

import Geisha.AST
import Geisha.Error

type Name = String

typeInt, typeBool, typeFloat, typeStr :: GType
typeInt = TCon "Int"
typeBool = TCon "Bool"
typeFloat = TCon "Float"
typeStr = TCon "Str"

arrow, product :: GType -> GType -> GType
arrow = TComb "->"
product = TComb "*"

productAll = foldl1 product

newtype TypeEnv = TypeEnv (M.Map Name Scheme)
                deriving (Eq, Show)

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (n, s) = TypeEnv $ M.insert n s env

type Subst = M.Map Name Type

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

composeAll = foldl1 compose

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set

instance Substitutable GType where
  apply _ (TCon a) = TCon a
  apply s t@(TVar a) = M.findWithDefault t a s
  apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2

  ftv (TCon _) = S.empty
  ftv (TVar a) = S.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where s' = foldr M.delete s as
  ftv (Forall as t) = ftv t S.\\ S.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv e) = TypeEnv $ M.map (apply s) e
  ftv (TypeEnv e)     = ftv $ M.elems e

type Infer a = ExceptT TypeError (State Unique) a

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Right res -> Right $ closeOver res
  err       -> err


letters = [String]
letters = [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer GType
fresh = do
  s <- get
  put $ s { count = count s + 1 }
  return . TVar $ letters !! count s

occursCheck :: Substitutable a => TVar -> a -> Bool
occursCheck a t = a `S.member` ftv t

bind :: Name -> Type -> Infer Subst
bind a t
  | t == TVar a     = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise       = return $ M.singleton a t

unify :: GType -> GType -> Infer Subst
unify (TComb n l r) (TComb n' l' r') 
  | n == n' = do
      s1 <- unify l l'
      s2 <- unify (apply s1 r) (apply s1 r')
      return $ s2 `compose` s1

unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b)
  | a == b = return nullSubst
unify t1 t2 = throwError $ Mismatch t1 t2

instantiate :: Scheme -> Infer GType
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = M.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where as = S.toList $ ftv t S.\\ ftv env

lookupEnv :: Env -> Name -> Infer (Subst, Type)
lookupEnv (TypeEnv e) x = case x `M.lookup` e of
  Just t -> do
    t' <- instantiate t
    return (nullSubst, t')
  _      -> throwError $ NotInScope x

infer :: TypeEnv -> AST -> Infer (Subst, Type)
infer env (Expr pos ty ex) = case ex of
  Lit lit -> let
    constType ty = return (nullSubst, ty)
    in constType $ case lit of
      LInt{} -> typeInt
      LStr{} -> typeStr
      LBool{} -> typeBool
      LFloat{} -> typeFloat
  Var x -> lookupEnv env x
  Function (Lambda [] e) -> return (nullSubst, arrow Void $ infer env e)
  Function (Lambda x e) -> do
    tvs <- mapM (const fresh) x
    let tvargs = map (Forall []) tvs
    let nargs = map (\(Expr _ _ (Var n)) -> n) x
    -- env' <- foldl extender x env
    let env' = foldl extend env $ zip nargs tvargs
    -- let env' = env (`extend` (x, Forall [] tv))
    (s', t') <- infer env' e
    return (s', targs `arrow` t')
      where targs = productAll $ apply s' tvs
  
  Apply e [] -> do
    (s, t) <- infer env e
    tv <- fresh
    s <- unify t1 $ Void `arrow` tv
    return (s, apply s tv)
  Apply e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2s, t2s) <- unzip <$> mapM (infer $ apply s1 env) $ e2
    let s2 = composeAll s2s
    let t2 = productAll t2s
    s3 <- unify (apply s2 t1) $ t2 `arrow` tv
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  Let (Expr _ _ (Var x)) e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t'   = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s1 `compose` s2, t2)

  If cond tr fl -> do
    (s1, t1) <- infer env cond
    (s2, t2) <- infer env tr
    (s3, t3) <- infer env fl
    s4 <- unify t1 typeBool
    s5 <- unify t2 t3
    return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)