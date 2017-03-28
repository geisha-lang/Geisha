module Geisha.AST where

import Data.List
import Text.Parsec.Pos

type Name = String

data ASTNode a = ASTNode {
  _pos  :: SourcePos,
  _type :: GType,
  _form :: a
} deriving (Eq, Ord)


data Form = Expr (ASTNode Expr)

          | Decl (ASTNode Decl)
          deriving (Eq, Ord)

instance Show Form where
  show (Expr (ASTNode _ _ exp))  = show exp
  show (Decl (ASTNode _ _ decl)) = show decl

formType (Expr (ASTNode _ ty _)) = ty
formType (Decl (ASTNode _ ty _)) = ty


-- data Form = Expr SourcePos GType Expr
--           | Decl SourcePos GType Decl
--          deriving (Eq, Ord)

noType pos = Expr . ASTNode pos TSlot

data GType = TSlot
           | Void
           | TVar Name
           | TCon Name
           | TArr GType GType
           | TProd GType GType
           | Forall [Name] GType
           deriving (Eq, Ord)

showFact t@(TArr _ _ ) = "(" ++ show t ++ ")"
showFact t             = show t

instance Show GType where
  show TSlot = "_"
  show Void  = "∅"
  show (TVar n) = n
  show (TCon n) = n
  show (TArr l r)  = unwords [showFact l, "⇒", showFact r]
  show (TProd l r) = unwords [showFact l, "×", showFact r]
  show (Forall names ty) = unwords $ quantifier ++ [show ty]
    where quantifier = if null names then [] else ["∀", intercalate ", " names, "."]
-- data Scheme = Forall [Name] GType
            -- deriving (Eq, Ord)

-- instance Show Scheme where


data Expr = Lit Lit
          | Var Name
          | List [Form]
          | Block [Form]
          | Function Lambda
          | Let Form Form Form
          | If Form Form Form
          | Apply Form [Form]
          deriving (Eq, Show, Ord)

data Decl = Define Name Form
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
  params :: [Form],
  body :: Form
  -- closure :: Env
} deriving (Eq, Ord)

instance Show Lambda where
  show (Lambda p b) = "(" ++ intercalate ", " (map show p) ++ ") -> " ++ show b
