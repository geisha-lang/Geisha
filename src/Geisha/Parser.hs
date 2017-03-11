module Geisha.Parser (
    readExpr,
) where

import Data.Functor.Identity (Identity)

import qualified Text.Parsec.Expr as E
import Text.Parsec.Prim (getPosition)
import Text.ParserCombinators.Parsec (Parser, try, (<|>), (<?>),
                                      ParseError, choice,
                                      SourcePos, parse, many)

import Control.Applicative ((<$>))
import Control.Monad.Except

import Geisha.AST
import Geisha.Error
import qualified Geisha.Lexer as L

readExpr :: String -> ThrowsCompileErr [AST]
readExpr = readOrThrow program

readOrThrow :: Parser a -> String -> ThrowsCompileErr a
readOrThrow parser src = case parse parser "Geisha" src of
  Left err  -> throwError $ Parse err
  Right res -> return res


expr :: Parser AST
expr = lets <|> E.buildExpressionParser operatorTable factor'

unOpTable = [ map prefix [ "+", "-" ] ]
binOpTable = map assocLefts [ [ "*", "/", "%" ],
                              [ "+", "-" ],
                              [ ">", "<", "==" ],
                              [ "&&", "||" ],
                              [ "=" ] ]
  where assocLefts = map (`binary` E.AssocLeft)

operatorTable = unOpTable ++ binOpTable

binary :: String -> E.Assoc -> E.Operator String () Identity AST
binary tk = E.Infix $ do
  pos <- getPosition
  L.reservedOp tk
  return (\lhs rhs -> noType pos $ BinExpr tk lhs rhs)

prefix :: String -> E.Operator String () Identity AST
prefix tk = E.Prefix $ do
  pos <- getPosition
  L.reservedOp tk
  return (noType pos . UnExpr tk)
  -- return . Expr pos $ UnExpr tk

parseToAST parser = getPosition >>= \pos -> noType pos <$> parser


factor = parseToAST (choice [ ifelse, string, number, try bool,
                              identifier, list, try lambda,
                              block ]) <|> L.parens expr

factor' :: Parser AST
factor' = do
  pos <- getPosition
  fun <- factor
  rest pos fun
  where rest :: SourcePos -> AST -> Parser AST
        rest pos x = (do args <- L.parens $ L.commaSep expr
                         pos' <- getPosition
                         rest pos' . noType pos $ Apply x args) <|> return x


string :: Parser Expr
string = String <$> L.string

identifier :: Parser Expr
identifier = Ident <$> L.identifier

bool :: Parser Expr
bool = true <|> false
  where true = L.reserved "true" >> return (Bool True)
        false = L.reserved "false" >> return (Bool False)

float :: Parser Expr
float = Float <$> L.float

integer :: Parser Expr
integer = Integer <$> L.integer

number :: Parser Expr
number = try float <|> try integer

list :: Parser Expr
list = List <$> L.brackets (L.commaSep expr)

block :: Parser Expr
block = Block <$> L.braces (L.stmtSep expr)

lambda :: Parser Expr
lambda = do
  params <- L.parens . L.commaSep $ L.identifier
  L.reservedOp "->"
  body <- expr
  return . Function $ Lambda params body

ifelse :: Parser Expr
ifelse = do
  L.reserved "if"
  cond <- L.parens expr
  tr <- expr
  L.reserved "else"
  fl <- expr
  return $ If cond tr fl

assignment :: Parser (String, AST)
assignment = do
  var <- L.identifier
  L.reservedOp "="
  rhs <- expr
  return (var, rhs)

lets :: Parser AST
lets = do
  L.reserved "let"
  binds <- L.commaSep assignment
  L.reserved "in"
  target <- expr
  return $ foldr lett target binds
    where lett (var, b@(Expr t pos1 exp1)) =
            Expr t pos1 . Let var b

definition :: Parser AST
definition = parseToAST $ do
  L.reserved "def"
  bind <- assignment
  return . uncurry Define $ bind

entry :: Parser AST
entry = try definition <|> try expr

program :: Parser [AST]
program = L.lineSep entry
