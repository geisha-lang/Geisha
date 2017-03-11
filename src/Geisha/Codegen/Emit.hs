module Geisha.Codegen.Emit where

import Control.Monad
import Control.Monad.Except

import LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import LLVM.General.Module
import LLVM.General.Context

-- import LLVM.General.PassManager

import qualified Data.Map as M

import Geisha.Codegen.LLVM
import Geisha.Codegen.Primitive
import Geisha.AST as S

topLevel :: AST -> LLVM ()
topLevel (Expr _ _ (Define name (Expr _ _ (S.Function (Lambda args body))))) = define integer name fnargs bls
  where fnargs = toSig args
        bls = createBlocks . execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          forM_ args $ \a -> do
            var <- alloca integer
            store var . local $ AST.Name a
            assignVar a var
          gen body >>= ret

topLevel prog = define integer "main" [] blks
  where blks = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          gen prog >>= ret


toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map $ \x -> (integer, AST.Name x)

-- lambda :: AST -> Codegen AST.Operand

cons = ConstantOperand

gen :: S.AST -> Codegen AST.Operand
gen (S.Expr _ _ (S.Float f)) = return . cons . C.Float $ F.Double f
gen (S.Expr _ _ (S.Integer i)) = return . cons $ C.Int 32 i
gen (S.Expr _ _ (S.Ident v)) = getVar v >>= load
gen (S.Expr _ _ (S.Apply fn args)) = do
  largs <- mapM gen args
  call (externf integer $ AST.Name fun) largs
  where (S.Expr _ _ (S.Ident fun)) = fn

gen (S.Expr _ _ (S.BinExpr "=" (S.Expr _ _ (S.Ident var)) val)) = do
  a <- getVar var
  rhs <- gen val
  store a rhs
  return rhs

gen (S.Expr _ _ (S.BinExpr op l r)) =
  case M.lookup op binops of
    Just f -> do
      clhs <- gen l
      crhs <- gen r
      f clhs crhs
    Nothing -> error $ "No such operator '" ++ op ++ "'"

gen (S.Expr _ _ (S.Block exps)) = loop exps
  where loop [e] = gen e
        loop (e:es) = do
          gen e
          loop es

gen (S.Expr _ _ (S.If cond tr fl)) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cond <- gen cond
  test <- fcmp FP.ONE (cons $ C.Float (F.Double 0.0)) cond
  cbr test ifthen ifelse

  setBlock ifthen
  trval <- gen tr
  br ifexit
  ifthen <- getBlock

  setBlock ifelse
  flval <- gen fl
  br ifexit
  ifelse <- getBlock

  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]

gen ast = error $ "Not implement yet: \n" ++ show ast
-- passes :: PassSetSpec
-- passes = defaultCuratedPassSetSpec { optLevel = Just 3 }



liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.AST] -> IO AST.Module
codegen mod fns = withContext $ \ctx ->
  liftError $ withModuleFromAST ctx newAST $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newAST
  where modn = mapM topLevel fns
        newAST = runLLVM mod modn
