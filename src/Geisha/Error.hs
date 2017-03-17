module Geisha.Error (
    ThrowsCompileErr,
    IOThrowsError,
    CompileErr(..),
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
               | NotFunction Type
               | NotInScope Name
               | InfiniteType Name GType

instance Show CompileErr where
    show (Parse err) = show err
    show (Unbound name) = "Unbound identifier: " ++ name
    show (Default s) = "Error: " ++ s
    show (TypeMissMatch node expect actual) = "Type error: " ++
                                              show node ++
                                              " expect: " ++
                                              show expect ++
                                              ", actual: " ++
                                              show actual