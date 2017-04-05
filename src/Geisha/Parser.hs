{-# LANGUAGE FlexibleContexts #-}
module Geisha.Parser (
    readSource,
) where

import Data.Functor.Identity (Identity)

import qualified Text.Parsec.Expr as E
import Text.Parsec.Prim (getPosition)
import Text.ParserCombinators.Parsec (Parser, try, (<|>), (<?>),
                                      ParseError, choice,
                                      SourcePos, parse, many, eof,
                                      sourceLine, sourceColumn)

import Control.Applicative ((<$>))
import Control.Monad.Except

import Geisha.AST
import Geisha.Error
import qualified Geisha.Lexer as L

posToLoc pos = Located (sourceLine pos) (sourceColumn pos)

readSource :: (MonadError CompileErr m) => String -> m [Syntax]
-- readSource s = return <$> readOrThrow entry s
readSource = readOrThrow program

readOrThrow :: (MonadError CompileErr m) => Parser a -> String -> m a
readOrThrow parser src = case parse parser "Geisha" src of
  Left err  -> throwError $ Parse err
  Right res -> return res


expr :: Parser Syntax
expr = lets <|> E.buildExpressionParser operatorTable factor'

unOpTable  = [ map prefix [ "+", "-" ] ]
binOpTable = map assocLefts [ [ "*", "/", "%" ],
                              [ "+", "-" ],
                              [ ">", "<", "==" ],
                              [ "&&", "||" ],
                              [ "=" ] ]
  where assocLefts = map (`binary` E.AssocLeft)

operatorTable = unOpTable ++ binOpTable

parseOpApply :: Name -> Parser ([Syntax] -> Syntax)
parseOpApply tk = do
  pos <- posToLoc <$> getPosition
  L.reservedOp tk
  let identOp = bareExpr pos $ Var tk
  return $ \es -> bareExpr pos $ Apply identOp es

binary :: String -> E.Assoc -> E.Operator String () Identity Syntax
binary tk = E.Infix $ do
  op <- parseOpApply tk
  return (\lhs rhs -> op [lhs, rhs])

prefix :: String -> E.Operator String () Identity Syntax
prefix tk = E.Prefix $ do
  op <- parseOpApply tk
  return (\e -> op [e])

parseToAST parser = posToLoc <$> getPosition >>= \pos -> bareExpr pos <$> parser


factor = parseToAST (choice [ ifelse,         -- | the `if` `else` expression
                              string,         -- | the string literal
                              number,         -- | number and float literal
                              try bool,       -- | boolean value
                              try lambda,     -- | lambda expression
                              identifier,     -- | variable identifier
                              list,           -- | list (vector/array)
                              block           -- | a sequence of expression
                            ]) <|> L.parens expr

factor' :: Parser Syntax
factor' = do
  pos <- posToLoc <$> getPosition
  fun <- factor
  rest pos fun
  where rest :: Loc -> Syntax -> Parser Syntax
        rest pos x = (do args <- L.parens $ L.commaSep expr
                         pos' <- posToLoc <$> getPosition
                         rest pos' . bareExpr pos $ Apply x args) <|> return x


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

assignment :: Parser (Syntax, Syntax)
assignment = do
  var <- parseToAST identifier
  L.reservedOp "="
  rhs <- expr
  return (var, rhs)

lets :: Parser Syntax
lets = do
  L.reserved "let"
  binds <- L.commaSep assignment
  L.reserved "in"
  target <- expr
  return $ foldr lett target binds
    where lett (var, b@(Expr (Annotation pos1 t) exp1)) =
            Expr (Annotation pos1 t) . Let var b

definition :: Parser Syntax
definition = posToLoc <$> getPosition >>= \pos -> Decl (Annotation pos (Forall [] $ TSlot pos)) <$> do
  L.reserved "def"
  -- bind <- assignment
  var <- L.identifier
  L.reservedOp "="
  rhs <- expr
  return $ Define var rhs

entry :: Parser Syntax
entry = try definition <|> try expr

contents :: Parser a -> Parser a
contents p = do
  L.whiteSpace
  r <- p
  eof
  return r

program :: Parser [Syntax]
program = contents $ L.lineSep entry
-- program = contents . many $ do
--   e <- entry
--   L.lineBreaker
--   return e
