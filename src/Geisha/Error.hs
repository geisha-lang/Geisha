module Geisha.Error (
    ThrowsCompileErr,
    CompileErr(..)
) where

import Text.Parsec.Error (ParseError)

import Geisha.AST

type ThrowsCompileErr = Either CompileErr

data CompileErr = Parse ParseError
                | Unbound Expr

instance Show CompileErr where
    show (Parse err) = show err
    show (Unbound (Ident id)) = "Unbound identifier: " ++ id