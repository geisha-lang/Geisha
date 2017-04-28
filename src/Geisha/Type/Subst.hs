module Geisha.Type.Subst where

import qualified Data.HashMap    as M
import           Geisha.AST

import qualified Data.Set        as S
import           Geisha.Type.Env

type Subst = M.Map Name GType


emptySubst :: Subst
emptySubst = M.empty


-- | The Substituable type class
class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> S.Set Name


-- | Do subst on type
instance Substitutable Syntax where
  apply s (Expr a@(Annotation _ ty) ex) =
    Expr (a { _type = apply s ty }) $
      apply s ex
  apply s (Decl a@(Annotation _ ty) decl) =
    Decl (a { _type = apply s ty }) $
      apply s decl
  ftv (Expr (Annotation _ ty) _) =
    ftv ty
  ftv (Decl (Annotation _ ty) _) =
    ftv ty


instance Substitutable Expr where
  apply s (List es) = List $ apply s es
  apply s (Block es) = Block $ apply s es
  apply s (Function (Lambda ps b)) =
    Function $ Lambda (apply s ps) (apply s b)
  apply s (Let n v e) = Let (apply s n) (apply s v) (apply s e)
  apply s (If cond tr tf) =
    If (apply s cond) (apply s tr) (apply s tf)
  apply s (Apply f args) =
    Apply (apply s f) $ apply s args
  apply _ e = e


  ftv (List es) = ftv es
  ftv (Block es) = ftv es
  ftv (Function (Lambda ps b)) =
    ftv ps `S.union` ftv b
  ftv (Let n v e) = ftv n `S.union` ftv v `S.union` ftv e
  ftv (If cond tr tf) = ftv cond `S.union` ftv tr `S.union` ftv tf
  ftv (Apply f args) = ftv f `S.union` ftv args
  ftv _ = S.empty


instance Substitutable Decl where
  apply s (Define n syn) =
    Define n $ apply s syn
  apply _ d = d
  ftv (Define _ syn) = ftv syn
  ftv t = S.empty


-- | Subst type varables
instance Substitutable GType where
  apply _ v@(Void _) = v
  apply _ c@(TCon l a) = c
  apply s t@(TVar _ a) = M.findWithDefault t a s
  apply s (TArr pos t1 t2) =
    TArr pos (apply s t1) $ apply s t2
  apply s (TProd pos t1 t2) =
    TProd pos (apply s t1) $ apply s t2
  apply _ t = t
  ftv (Void _) = S.empty
  ftv (TCon _ _) = S.empty
  ftv (TVar _ a) = S.singleton a
  ftv (TArr _ t1 t2) = ftv t1 `S.union` ftv t2
  ftv (TProd _ t1 t2) = ftv t1 `S.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr M.delete s as
  ftv (Forall as t) = ftv t S.\\ S.fromList as

instance Substitutable a =>
         Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv e) = TypeEnv $ M.map (apply s) e
  ftv (TypeEnv e) = ftv $ M.elems e
