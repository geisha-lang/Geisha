module Geisha.AST where

import Data.List
-- import Text.Parsec.Pos

type Name = String

data Loc = NoLoc | Located Int Int
         deriving (Eq, Ord)


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

instance Show Syntax where
  show (Expr _ exp)  = show exp
  show (Decl _ decl) = show decl

bareExpr pos = Expr $ Annotation pos . Forall [] $ TSlot pos

varName (Expr _ (Var n)) = n

syntaxType (Expr (Annotation _ ty) _) = ty
syntaxType (Decl (Annotation _ ty) _) = ty

-- data Syntax = Expr (ASTNode Expr)
--           | Decl (ASTNode Decl)
--           deriving (Eq, Ord)

-- instance Show Syntax where
--   show (Expr (ASTNode _ _ exp))  = show exp
--   show (Decl (ASTNode _ _ decl)) = show decl

syntaxType (Expr (Annotation _ ty) _) = ty
syntaxType (Decl (Annotation _ ty) _) = ty


-- data Syntax = Expr Loc GType Expr
--           | Decl Loc GType Decl
--          deriving (Eq, Ord)

-- noType pos = Expr . ASTNode pos TSlot


data GType = TSlot Loc
           | Void Loc
           | TVar Loc Name
           | TCon Loc Name
           | TArr Loc GType GType
           | TProd Loc GType GType
          --  | Forall Loc [Name] GType
           deriving (Eq, Ord)

showFact t@TArr{} = "(" ++ show t ++ ")"
showFact t             = show t

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

instance Show GType where
  show (TSlot _)     = "_"
  show (Void _)      = "∅"
  show (TVar _ n)    = n
  show (TCon _ n)    = n
  show (TArr _ l r)  = unwords [showFact l, "⇒", showFact r]
  show (TProd _ l r) = unwords [showFact l, "×", showFact r]
  -- show (Forall names ty) = unwords $ quantifier ++ [show ty]
  --   where quantifier = if null names then [] else ["∀", intercalate ", " names, "."]

data Scheme = Forall [Name] GType
            deriving (Eq, Ord)

instance Locatable Scheme where
  setLoc (Forall as t) = Forall as . setLoc t
  getLoc (Forall _ t)  = getLoc t

instance Show Scheme where
  show (Forall names ty) = unwords $ quantifier ++ [show ty]
    where quantifier = if null names then [] else ["∀", intercalate ", " names, "."]



data Expr = Lit Lit
          | Var Name
          | List [Syntax]
          | Block [Syntax]
          | Function Lambda
          | Let Syntax Syntax Syntax
          | If Syntax Syntax Syntax
          | Apply Syntax [Syntax]
          deriving (Eq, Show, Ord)


data Decl = Define Name Syntax
          | Concept {
            _name :: Name,
            _bounds :: [Decl]
          }
          | Inst Name 
          deriving (Eq, Show, Ord)

data Lit = LInt Integer
         | LFloat Double
         | LStr String
         | LBool Bool
         deriving (Eq, Show, Ord)


data Lambda = Lambda {
  params :: [Syntax],
  body :: Syntax
  -- closure :: Env
} deriving (Eq, Ord)

instance Show Lambda where
  show (Lambda p b) = "(" ++ intercalate ", " (map show p) ++ ") -> " ++ show b
