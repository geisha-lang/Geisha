module Geisha.AST where

import Data.List
import Text.Parsec.Pos

type Name = String

data AST = Expr SourcePos GType Expr
         | Decl SourcePos GType Decl
         deriving (Eq, Ord)

noType pos = Expr pos TSlot

data GType = TSlot
           | Void
           | TVar Name
           | TCon Name
          --  | TPrim PrimType
          --  | TArr GType GType
          --  | TProd GType GType
           | TComb Name GType GType
           | TConcept Name
            --  | TypeName Name
            --  | Concept Name
           deriving (Show, Eq, Ord)
-- data PrimType = TInt | TFloat | TBool | TStr | TArr | TProd
--               deriving (Show, Eq, Ord)

data Scheme = Forall [Name] GType
            deriving (Show, Eq, Ord)

-- data Type = TVar TVar | TCon TyCon | TApp Type Type | TArr Type Type
-- | TForall [Pred] [TVar] Type
instance Show AST where
  show (Expr _ _ exp)  = show exp
  show (Decl _ _ decl) = show decl

data Expr = Lit Lit
          | Var Name
          | List [AST]
          | Block [AST]
          -- | BinExpr Name AST AST
          -- | UnExpr Name AST
          | Function Lambda
          -- | Define AST AST
          | Let AST AST AST
          | If AST AST AST
          | Apply AST [AST]
          deriving (Eq, Show, Ord)

data Decl = Define Name AST
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

-- instance Show Expr where
--   show (Let name bind target) = "(let " ++ name ++ " = " ++ show bind ++ " in\n" ++ show target ++ ")"
--   show (Define name bind) = "(define " ++ name ++ " " ++ show bind ++ ")"
--   show (BinExpr op l r) = "(" ++ unwords [op, show l, show r] ++ ")"
--   show (UnExpr op e) = "(" ++ unwords [op, show e] ++ ")"
--   show (Function f) = show f
--   show (Apply f p) = "(" ++ show f ++ " " ++ intercalate ", " (map show p) ++ ")"
--   show (Block a) = "{\n" ++ unlines (map show a) ++ "}"
--   show (If cond t f) = "(if (" ++ show cond ++ ") \n" ++ show t ++ show f ++ ")"
--   show (Ident i) = i
--   show (List l)  = show l
--   show (Integer l)  = show l
--   show (Float l)  = show l
--   show (String l)  = show l
--   show (Bool l)  = show l


data Lambda = Lambda {
  params :: [AST],
  body :: AST
  -- closure :: Env
} deriving (Eq, Ord)

instance Show Lambda where
  show (Lambda p b) = "(" ++ intercalate ", " (map show p) ++ ") -> " ++ show b
