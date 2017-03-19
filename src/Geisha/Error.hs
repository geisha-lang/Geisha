module Geisha.Error (
    ThrowsCompileErr,
    IOThrowsError,
    CompileErr(..),
    TypeError(..),
    liftThrows
) where

import Control.Monad.Except

import Text.Parsec.Error (ParseError)

import Geisha.AST

type ThrowsCompileErr = Either CompileErr

type IOThrowsError = ExceptT CompileErr IO


liftThrows :: ThrowsCompileErr a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

data CompileErr = Parse ParseError
                | Unbound String
                | Default String
                | BadSpecialForm String Expr
                | TypeError TypeError

data TypeError = Mismatch GType GType
               | NotFunction GType
               | NotInScope Name
               | InfiniteType Name GType
               | Reserved String Expr
               | BadTypeForm String Expr
               | UnificationMismatch [GType] [GType]
               deriving (Show)

instance Show CompileErr where
    show (Parse err) = show err
    show (Unbound name) = "Unbound identifier: " ++ name
    show (Default s) = "Error: " ++ s
    show (TypeError err) = "Type error: " ++ show err