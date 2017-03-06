module Geisha.Codegen.Emit where

import Control.Monad
import Control.Monad.Except

import LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

-- import LLVM.General.Prelude
-- import LLVM.General.Context
import qualified Data.HashMap as M

import Geisha.Codegen.LLVM
import Geisha.Codegen.Primitive
import Geisha.AST as S

topLevel :: AST -> LLVM ()
topLevel (Expr _ _ (Define name (Lambda args body))) = define integer name fnargs bls
  where fnargs = toSig args
        bls = createBlocks . execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          forM_ args $ \a -> do
            var <- alloca integer
            store var . local $ AST.Name a
            assignVar a var
          codegen body >> ret

topLevel (Expr _ _ exp) = define integer "main" [] blks
  where blks = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          codegen exp >>= ret


toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map $ \x -> (integer, AST.Name x)

cons = ConstantOperand

gen :: S.AST -> Codegen AST.Operand
gen (S.Expr _ _ (S.Float f)) = return . cons $ C.Float f
gen (S.Expr _ _ (S.Integer i)) = return . cons $ C.Int i
gen (S.Expr _ _ (S.Ident v)) = getVar v >>= load
gen (S.Expr _ _ (S.Apply fn args)) = do
  largs <- mapM gen args
  call (externf integer $ AST.Name fun) largs
  where (S.Expr _ _ (S.Ident fun)) = fn

gen (S.Expr _ _ (S.BinExpr op l r)) = case M.lookup op binops of
    Just f -> do
      clhs <- gen l
      crhs <- gen r
      f clhs crhs
    Nothing -> error "No such operator '" ++ op ++ "'"

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