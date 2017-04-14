module Geisha.AST (
  Name,
  Loc(..),
  Locatable(..),
  Annotation(..),
  Syntax(..),
  GType(..),
  Scheme(..),
  Expr(..),
  Decl(..),
  Lit(..),
  Lambda(..),
  bareExpr,
  varName,
  syntaxType
) where

import           Data.List
-- import Text.Parsec.Pos

-- import Geisha.AST.PrettyPrint

type Name = String

data Loc = NoLoc | Located Int Int
         deriving (Eq, Ord)

instance Show Loc where
  show NoLoc = "Unknown loc"
  show (Located line col) = "line: " ++ show line ++ " col: " ++ show col

class Locatable a where
  getLoc :: a -> Loc
  setLoc :: a -> Loc -> a

data Annotation = Annotation {
  _pos  :: Loc,
  _type :: Scheme
} deriving (Eq, Ord)

instance Locatable Annotation where
  getLoc (Annotation l _) = l
  setLoc ann l = ann { _pos = l }

data Syntax = Expr Annotation Expr
            | Decl Annotation Decl
            deriving (Eq, Ord)

instance Locatable Syntax where
  getLoc (Expr a _) = getLoc a
  getLoc (Decl a _) = getLoc a

  setLoc (Expr a e) l = Expr (setLoc a l) e
  setLoc (Decl a e) l = Decl (setLoc a l) e



bareExpr pos = Expr $ Annotation pos . Forall [] $ TSlot pos

varName (Expr _ (Var n)) = n

syntaxType (Expr (Annotation _ ty) _) = ty
syntaxType (Decl (Annotation _ ty) _) = ty


data GType = TSlot Loc
           | Void Loc
           | TVar Loc Name
           | TCon Loc Name
           | TArr Loc GType GType
           | TProd Loc GType GType
           | TComp Loc GType GType
          --  | Forall Loc [Name] GType
           deriving (Eq, Ord)

-- showFact t@TArr{} = "(" ++ show t ++ ")"
-- showFact t             = show t

instance Locatable GType where
  setLoc (TSlot _) loc       = TSlot loc
  setLoc (Void _) loc        = Void loc
  setLoc (TVar _ n) loc      = TVar loc n
  setLoc (TCon _ n) loc      = TCon loc n
  setLoc (TArr _ t1 t2) loc  = TArr loc t1 t2
  setLoc (TProd _ t1 t2) loc = TProd loc t1 t2

  getLoc (TSlot loc)     = loc
  getLoc (Void loc)      = loc
  getLoc (TVar loc _)    = loc
  getLoc (TCon loc _)    = loc
  getLoc (TArr loc _ _)  = loc
  getLoc (TProd loc _ _) = loc

data Scheme = Forall [Name] GType
            deriving (Eq, Ord)

instance Locatable Scheme where
  setLoc (Forall as t) = Forall as . setLoc t
  getLoc (Forall _ t)  = getLoc t

data Expr = Lit Lit
          | Var Name
          | List [Syntax]
          | Block [Syntax]
          | Function Lambda
          | Let Syntax Syntax Syntax
          | If Syntax Syntax Syntax
          | Apply Syntax [Syntax]
          deriving (Eq, Ord)


data Decl = Define Name Syntax
          | Concept {
            _name   :: Name,
            _bounds :: [Decl]
          }
          | Inst Name
          deriving (Eq, Ord)

data Lit = LInt Integer
         | LFloat Double
         | LStr String
         | LBool Bool
         deriving (Eq, Ord)


data Lambda = Lambda {
  params :: [Syntax],
  body   :: Syntax
  -- closure :: Env
} deriving (Eq, Ord)


