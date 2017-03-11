module Geisha.Lexer where

import Text.Parsec.Char
import Text.Parsec.Prim ((<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec (try, sepBy, sepEndBy, many)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser defs
  where ops = [ "=", "*", "/", "%", "+", "-", ">", "<", "==", "&&", "||", "->" ]
        keys = ["if", "else", "let", "def", "true", "false", "in"]
        defs = emptyDef {
                 Tok.commentLine = ";;",
                 Tok.reservedOpNames = ops,
                 Tok.reservedNames = keys,
                 Tok.identStart = letter <|> oneOf "_$",
                 Tok.identLetter = alphaNum <|> oneOf "_$"
               }

whiteSpace = Tok.whiteSpace lexer

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

string :: Parser String
string = Tok.stringLiteral lexer

semi = Tok.semi lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

lexeme = Tok.lexeme lexer
lineBreaker = lexeme endOfLine

stmtSep :: Parser a -> Parser [a]
stmtSep p = sepBy p ((return <$> try lineBreaker) <|> semi)

lineSep :: Parser a -> Parser [a]
lineSep p = sepBy p lineBreaker
-- lineSep

