module Geisha.Parser (
    readExpr,
) where

import Data.Functor.Identity (Identity)

import qualified Text.Parsec.Expr as E
import Text.Parsec.Prim (getPosition)
import Text.ParserCombinators.Parsec (Parser, try, (<|>), (<?>),
                                      ParseError, choice,
                                      SourcePos, parse, many, eof)

import Control.Applicative ((<$>))
import Control.Monad.Except

import Geisha.AST
import Geisha.Error
import qualified Geisha.Lexer as L

readExpr :: String -> ThrowsCompileErr [Form]
-- readExpr s = return <$> readOrThrow entry s
readExpr = readOrThrow program

readOrThrow :: Parser a -> String -> ThrowsCompileErr a
readOrThrow parser src = case parse parser "Geisha" src of
  Left err  -> throwError $ Parse err
  Right res -> return res


expr :: Parser Form
expr = lets <|> E.buildExpressionParser operatorTable factor'

unOpTable = [ map prefix [ "+", "-" ] ]
binOpTable = map assocLefts [ [ "*", "/", "%" ],
                              [ "+", "-" ],
                              [ ">", "<", "==" ],
                              [ "&&", "||" ],
                              [ "=" ] ]
  where assocLefts = map (`binary` E.AssocLeft)

operatorTable = unOpTable ++ binOpTable

binary :: String -> E.Assoc -> E.Operator String () Identity Form
binary tk = E.Infix $ do
  pos <- getPosition
  L.reservedOp tk
  let identOp = noType pos $ Var tk
  return (\lhs rhs -> noType pos $ Apply identOp [lhs, rhs])

prefix :: String -> E.Operator String () Identity Form
prefix tk = E.Prefix $ do
  pos <- getPosition
  L.reservedOp tk
  let identOp = noType pos $ Var tk
  return (\e -> noType pos $ Apply identOp [e])
  -- return . Expr pos $ UnExpr tk

parseToAST parser = getPosition >>= \pos -> noType pos <$> parser


factor = parseToAST (choice [ ifelse, string, number, try bool,
                              try lambda, identifier, list,
                              block ]) <|> L.parens expr

factor' :: Parser Form
factor' = do
  pos <- getPosition
  fun <- factor
  rest pos fun
  where rest :: SourcePos -> Form -> Parser Form
        rest pos x = (do args <- L.parens $ L.commaSep expr
                         pos' <- getPosition
                         rest pos' . noType pos $ Apply x args) <|> return x


string :: Parser Expr
string = Lit . LStr <$> L.string

identifier :: Parser Expr
identifier = Var <$> L.identifier

bool :: Parser Expr
bool = true <|> false
  where true = L.reserved "true" >> return (Lit . LBool $ True)
        false = L.reserved "false" >> return (Lit . LBool $ False)

float :: Parser Expr
float = Lit . LFloat <$> L.float

integer :: Parser Expr
integer = Lit . LInt <$> L.integer

number :: Parser Expr
number = try float <|> try integer

list :: Parser Expr
list = List <$> L.brackets (L.commaSep expr)

block :: Parser Expr
block = Block <$> L.braces (L.stmtSep expr)

lambda :: Parser Expr
lambda = do
  params <- (L.parens . L.commaSep $ parseToAST identifier) <|> (return <$> parseToAST (try identifier))
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

assignment :: Parser (Form, Form)
assignment = do
  var <- parseToAST identifier
  L.reservedOp "="
  rhs <- expr
  return (var, rhs)

lets :: Parser Form
lets = do
  L.reserved "let"
  binds <- L.commaSep assignment
  L.reserved "in"
  target <- expr
  return $ foldr lett target binds
    where lett (var, b@(Expr t pos1 exp1)) =
            Expr t pos1 . Let var b

definition :: Parser Form
definition = getPosition >>= \pos -> Decl pos TSlot <$> do
  L.reserved "def"
  -- bind <- assignment
  var <- L.identifier
  L.reservedOp "="
  rhs <- expr
  return $ Define var rhs

entry :: Parser Form
entry = try definition <|> try expr

contents :: Parser a -> Parser a
contents p = do
  L.whiteSpace
  r <- p
  eof
  return r

program :: Parser [Form]
program = contents $ L.lineSep entry
-- program = contents . many $ do
--   e <- entry
--   L.lineBreaker
--   return e
