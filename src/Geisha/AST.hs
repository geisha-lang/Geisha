module Geisha.AST (
  Expr (..),
  AST (..),
  ASTType (..),
  PrimType (..),
  Lambda (..),
  noType
) where

import Data.List
import Text.Parsec.Pos

data AST = Expr {
  pos :: SourcePos,
  astType :: ASTType,
  content :: Expr
} deriving (Eq)

noType pos = Expr pos Unknown 

data ASTType = Unknown
             | PrimType PrimType
             | ListType ASTType
             | FunctionType ASTType [ASTType]

data PrimType = I32 | F64 | Boolean

instance Show AST where
  show (Expr _ _ exp) = show exp

type Name = String
data Expr = Integer Integer
      | Float Double
      | String String
      | Bool Bool
      | Ident Name
      | List [AST]
      | Block [AST]
      | BinExpr Name AST AST
      | UnExpr Name AST
      | Function Lambda
      | Define Name AST
      | Let Name AST AST
      | Apply AST [AST]
      deriving (Eq)

instance Show Expr where
  show (Let name bind target) = "(let " ++ name ++ " = " ++ show bind ++ " in\n" ++ show target ++ ")"
  show (Define name bind) = "(define " ++ name ++ " " ++ show bind ++ ")"
  show (BinExpr op l r) = "(" ++ unwords [op, show l, show r] ++ ")"
  show (UnExpr op e) = "(" ++ unwords [op, show e] ++ ")"
  show (Function f) = show f
  show (Apply f p) = "(" ++ show f ++ intercalate ", " (map show p) ++ ")"
  show (Block a) = "{\n" ++ unlines (map show a) ++ "}"
  show (Ident i) = i
  show (List l)  = show l
  show (Integer l)  = show l
  show (Float l)  = show l
  show (String l)  = show l
  show (Bool l)  = show l


data Lambda = Lambda {
  params :: [AST],
  body :: AST
  -- closure :: Env
} deriving (Eq)

instance Show Lambda where
  show (Lambda p b) = "(" ++ intercalate ", " (map show p) ++ ") -> " ++ show b
