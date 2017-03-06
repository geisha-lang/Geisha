module Geisha.Pass.Flatten (
    flatten,
    Arg (..),
    Statement (..),
    Flattened,
    Varable,
    InstrOp
) where

import Control.Monad.State
import Data.Maybe

import qualified Data.Map as M
import qualified Data.Set as S

import Geisha.AST

type ValCount = Integer

stateToName :: ValCount -> String
stateToName s = "tmp." ++ show s

valFromState :: ValCount -> Integer
valFromState = id
nextState :: ValCount -> ValCount
nextState = (+ 1)

type ValCountMonad = State ValCount
genNextVal :: ValCountMonad String
genNextVal = state (\st -> let st' = nextState st in (stateToName st, st'))


type Varable = String
type InstrOp = String
data Arg = Var Varable
         | Int Integer
         | BinInstr InstrOp Arg Arg
         | UnInstr InstrOp Arg
         | Call Varable
         deriving (Eq)

instance Show Arg where
    show (Var v) = "(var " ++ v ++ ")"
    show (Int i) = "(int " ++ show i ++ ")"
    show (BinInstr op lhs rhs) = "(" ++ op ++ " " ++ show lhs ++ " " ++ show rhs ++ ")"
    show (UnInstr op a) = "(" ++ op ++ " " ++ show a ++ ")"

data Statement = Assign Arg Arg
               | Return Arg
               deriving (Eq)

instance Show Statement where
    show (Assign lhs rhs) = "(assign " ++ show lhs ++ " " ++ show rhs ++ ")"

-- type 

type Flattened = (Arg, [Statement], [Varable])

flatten :: AST -> Maybe Arg -> ValCountMonad Flattened
flatten (Expr _ (Integer e)) (Just t@(Var v)) = return (t, [Assign (Int e) t], [v])
flatten (Expr _ (Integer e)) Nothing          = return (Int e, [], [])

flatten (Expr _ (Ident ident)) (Just t@(Var v)) = return (t, [Assign (Var ident) t], [v])
flatten (Expr _ (Ident ident)) Nothing          = return (Var ident, [], [])

flatten (Expr _ (BinExpr op lhs rhs)) target = do
    (lRes, lAss, lVars) <- flatten lhs Nothing
    (rRes, rAss, rVars) <- flatten rhs Nothing
    val <- case target of
                Nothing  -> genNextVal
                (Just (Var t)) -> return t

    return (Var val,
            lAss ++ rAss ++ [Assign (Var val) (BinInstr op lRes rRes)],
            lVars ++ rVars ++ if isJust target then [] else [val])
