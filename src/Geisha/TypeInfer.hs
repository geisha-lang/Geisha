{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Geisha.TypeInfer where

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.HashMap           as M
import           Data.List              (last, nub)
import           Data.Maybe
import qualified Data.Set               as S
import           Prelude                hiding (product)
-- import Control.Monad.RWS
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Geisha.AST
import           Geisha.Error
import           Geisha.Type.Env
import           Geisha.Type.Subst

typeInt, typeBool, typeFloat, typeStr :: Loc -> GType
typeInt loc = TCon loc "Int"

typeBool loc = TCon loc "Bool"

typeFloat loc = TCon loc "Float"

typeStr loc = TCon loc "Str"

arrow, product :: Loc -> GType -> GType -> GType
arrow = TArr

product = TProd

productMany :: Loc -> [GType] -> GType
productMany loc [] = Void loc
productMany loc ts = setLoc (foldl1 (TProd loc) ts) loc


data InferState = InferState { count :: Int, constraint :: [Constraint] }

initInfer = InferState 0 []

-- type Infer a = ExceptT TypeError (State Unique) a
type Infer = ReaderT TypeEnv (StateT InferState (Except TypeError))

type Constraint = (GType, GType)

instance Substitutable Constraint where
  apply s (t1, t2) = (apply s t1, apply s t2)
  ftv (t1, t2) = ftv t1 `S.union` ftv t2


type Unifier = (Subst, [Constraint])

type Solve = Except TypeError

runSolve :: (MonadError TypeError m) => [Constraint] -> m Subst
runSolve cs = either throwError return . runExcept $ solver (emptySubst, cs)

runInfer :: (MonadError TypeError m)
         => TypeEnv
         -> Infer a
         -> m (a, [Constraint])
runInfer env m = do
  (res, s) <- either throwError return . runExcept $
                runStateT (runReaderT m env) initInfer
  return (res, constraint s)

closeOver :: GType -> Scheme
closeOver = normalize . generalize emptyEnv

-- | make type var names like `a0`, `a1` normalize to `a`, `b`
normalize :: Scheme -> Scheme
normalize (Forall _ body) =
  Forall (map snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) letters    -- generated type names
    fv (Void _) = []
    fv (TCon _ _) = []
    fv (TVar _ a) = [a]
    fv (TArr _ a b) = fv a ++ fv b
    fv (TProd _ a b) = fv a ++ fv b
    normtype t@Void{} = t
    normtype (TArr l a b) = TArr l (normtype a) (normtype b)
    normtype (TProd l a b) =
      TProd l (normtype a) (normtype b)
    normtype c@TCon{} = c
    normtype (TVar l a) =
      case Prelude.lookup a ord of
        Just x  -> TVar l x
        Nothing -> error "type variable not in signature"

generalize :: TypeEnv -> GType -> Scheme
generalize env t = Forall as t
  where
    as = S.toList $ ftv t S.\\ ftv env

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Loc -> Infer GType
fresh loc = do
  s <- get
  put $
    s { count = count s + 1 }
  return . TVar loc $ letters !! count s

uni :: GType -> GType -> Infer ()
uni tl tr = do
  cs <- constraint <$> get
  modify $
    \s -> s { constraint = cs ++ [(tl, tr)] }

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
  scm <- lookupEnvScm x
  instantiate scm

lookupEnvScm :: Name -> Infer Scheme
lookupEnvScm x = do
  (TypeEnv env) <- ask
  -- fromMaybe (throwError $ NotInScope x) (M.lookup x env)
  case M.lookup x env of
    Nothing  -> throwError $ NotInScope x
    Just scm -> return scm

inferTop, inferDecls :: [Syntax] -> Infer ([Syntax], TypeEnv)
-- | Add all declarations' name to Env | If no type specified, it will be
inferTop ds = do
  tvs <- mapM (fresh . getLoc . syntaxType) ds
  env <- ask
  let ns = map declName ds
      tvs' = map (generalize env) tvs
      pairs = zip ns tvs'
  inEnv' pairs $ inferDecls ds

  where
    declName (Decl _ (Define n _)) =
      n

-- | Inference declarations with the env
inferDecls [] = do
  env <- ask
  return ([], env)
inferDecls (Decl anno (Define name e):ds) = do
  (form, scm) <- inferExpr e
  (fs, env) <- inEnv (name, scm) $ inferDecls ds
  return (Decl (anno { _type = scm }) (Define name form) :
          fs, env)

inferExpr :: Syntax -> Infer (Syntax, Scheme)
inferExpr e = do
  env <- ask
  case runInfer env $ infer e of
    Right ((Expr anno ex, ty), cs) ->
      case runSolve cs of
        Left err -> throwError err
        Right subst -> do
          let scm = closeOver $ apply subst ty
          return (Expr (anno { _type = scm }) $
                    apply subst ex, scm)
    Left err -> throwError err

infer :: Syntax -> Infer (Syntax, GType)
infer (Expr anno@(Annotation pos ty) ex) =
  let typeNode :: Expr -> GType -> Infer (Syntax, GType)
      typeNode exp ty = do
                          env <- ask
                          let cty = generalize env ty
                          return (Expr (anno { _type = cty }) exp, ty)
      withType = typeNode ex
  in case ex of
    Lit lit -> withType $ ty pos
      where ty =
              case lit of
                LInt{}   -> typeInt
                LStr{}   -> typeStr
                LBool{}  -> typeBool
                LFloat{} -> typeFloat
    Var x -> do
      -- ty <- lookupEnvScm x ins <- instantiate ty
      env <- ask
      ty <- lookupEnv x
      -- let scm = generalize env ty
      let scm = Forall [] ty
      return (Expr (anno { _type = scm }) ex, ty)
    Function (Lambda xs e) -> do
      let forall = Forall []
          nargs = map varName xs
      tvs <- mapM (fresh . getLoc) xs
      let targs = productMany pos tvs
          scms = map forall tvs
          xs' = map (\(Expr anno ex, s) -> Expr (anno { _type = s }) ex) $
            zip xs scms
      (body, tbody) <- inEnv' (zip nargs scms) (infer e)
      typeNode (Function $ Lambda xs' body) $ arrow pos targs tbody
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
      cs <- constraint <$> get
      case runSolve cs of
        Left err -> throwError err
        Right sub -> do
          let sc = generalize (apply sub env) (apply sub te1)
          (lete, tlete) <- inEnv (x, sc) $
                             local (apply sub) (infer e2)
          let ctlete = generalize env tlete
          return (Expr (anno { _type = ctlete }) $
                    Let (Expr (annov { _type = sc }) letv) e1 lete, tlete)
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
-- FUCK.... 打错一行卡了一个星期 太相信类型系统带来的正确性了，对于这种低级错误无能为力…… MMP 害得我一个星期没有好好复习数学 bind a
-- TVar{} = return emptySubst
bind a t
  | t `eqNoLoc` TVar NoLoc a =
      return emptySubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ M.singleton a t

eqNoLoc :: GType -> GType -> Bool
eqNoLoc (TArr _ l1 l2) (TArr _ r1 r2) =
  eqNoLoc l1 r1 && eqNoLoc l2 r2
eqNoLoc (TProd _ l1 l2) (TProd _ r1 r2) =
  eqNoLoc l1 r1 && eqNoLoc l2 r2
eqNoLoc l r = setLoc l NoLoc == setLoc r NoLoc

unifies :: GType -> GType -> Solve Subst
unifies t1 t2
  | eqNoLoc t1 t2 = return emptySubst
unifies (TVar _ l) r@TVar{} =
  l `bind` r
unifies (TVar _ v) t = v `bind` t
unifies t (TVar _ v) = v `bind` t
unifies (TArr _ t1 t2) (TArr _ t3 t4) =
  unifyMany [t1, t2] [t3, t4]
unifies (TProd _ t1 t2) (TProd _ t3 t4) =
  unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ Mismatch t1 t2

unifyMany :: [GType] -> [GType] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1:ts1) (t2:ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return $ su2 `compose` su1
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

-- compose = M.union
composeMany :: [Subst] -> Subst
composeMany = foldl1 compose

solver :: Unifier -> Solve Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((t1, t2):cs0) -> do
      su1 <- unifies t1 t2
      solver (su1 `compose` su, apply su1 cs0)
