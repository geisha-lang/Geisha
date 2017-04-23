{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Geisha.Error (
    ThrowsCompileErr,
    ThrowsTypeError,
    IOThrowsError,
    CompileErr(..),
    TypeError(..),
    liftTypeError,
    liftThrows
) where

import           Control.Monad.Except

import           Text.Parsec.Error      (ParseError)

import           Geisha.AST
import           Geisha.AST.PrettyPrint

type ThrowsCompileErr = Either CompileErr

type ThrowsTypeError = Either TypeError

type IOThrowsError = ExceptT CompileErr IO


liftThrows :: ThrowsCompileErr a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


liftTypeError :: (MonadError CompileErr mc) => ThrowsTypeError a -> mc a
liftTypeError = either (throwError . TypeError) return

data CompileErr = Parse ParseError
                | Unbound String
                | Default String
                | BadSpecialForm String Syntax
                | TypeError TypeError

data TypeError = Mismatch GType GType
               | NotFunction GType
               | NotInScope Name
               | InfiniteType Name GType
               | Reserved String
               | BadTypeForm String Expr
               | UnificationMismatch [GType] [GType]
            --    deriving (Show)
instance Show TypeError where
    show (Mismatch tl tr) = "Type mismatch: " ++ show tl ++ " in " ++ show (getLoc tl) ++ " with " ++ show tr ++ " in " ++ show (getLoc tr)
    show e = "Error"

instance Show CompileErr where
    show (Parse err) = show err
    show (Unbound name) = "Unbound identifier: " ++ name
    show (Default s) = "Error: " ++ s
    show (TypeError err) = "Type error: " ++ show err
