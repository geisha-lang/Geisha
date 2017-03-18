module Geisha.Codegen.Emit where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans

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
import qualified Geisha.Error as E


liftThrows :: E.ThrowsCompileErr a -> ExceptT E.CompileErr LLVM a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
-- liftCompileError :: Monad m => ExceptT E.CompileErr Codegen -> m a

extractIdent :: AST -> E.ThrowsCompileErr String
extractIdent (Expr _ _ (Var n)) = return n
extractIdent bad                  = throwError . E.Default $ "Illegel indentifier: " ++ show bad

topLevel :: AST -> ExceptT E.CompileErr LLVM ()
topLevel (Decl _ _ (Define name (Expr _ _ (S.Function (Lambda args body))))) = do
  args <- liftThrows $ mapM extractIdent args
  -- name <- liftThrows $ extractIdent name
  bls <- liftGen $ do
    entry <- addBlock entryBlockName
    setBlock entry
    forM_ args $ \a -> do
      var <- alloca integer
      store var . local $ AST.Name a
      assignVar a var
    gen body >>= ret
  lift $ define integer name (toSig args) bls

topLevel prog = blks >>= lift . define integer "main" []
  where blks = liftGen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          gen prog >>= ret

liftGen g = liftThrows $ execCodegen (runExceptT g) >>= createBlocks

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map $ \x -> (integer, AST.Name x)

cons = ConstantOperand

gen :: S.AST -> CodegenErrorT AST.Operand
gen (S.Expr _ _ (S.Lit lit)) = return . cons $ case lit of
  LInt i      -> C.Int 32 i
  LFloat f    -> C.Float $ F.Double f
  LBool True  -> C.Float $ F.Double 1.0
  LBool False -> C.Float $ F.Double 0.0
  -- return . cons . C.Float $ F.Double f
-- gen (S.Expr _ _ (S.Integer i)) = return . cons $ C.Int 32 i
gen (S.Expr _ _ (S.Var v)) = getVar v >>= load
gen (S.Expr _ _ (S.Apply fn args)) = do
  largs <- mapM gen args
  call (externf integer $ AST.Name fun) largs
  where (S.Expr _ _ (S.Var fun)) = fn

-- gen (S.Expr _ _ (S.BinExpr "=" (S.Expr _ _ (S.Var var)) val)) = do
--   a <- getVar var
--   rhs <- gen val
--   store a rhs
--   return rhs

-- gen (S.Expr _ _ (S.BinExpr op l r)) =
--   case M.lookup op binops of
--     Just f -> do
--       clhs <- gen l
--       crhs <- gen r
--       f clhs crhs
--     Nothing -> throwError . E.Default $ "No such operator '" ++ op ++ "'"

gen (S.Expr _ _ (S.Apply (S.Expr _ _ (S.Var "=")) [S.Expr _ _ (S.Var var), val])) = do
  a <- getVar var
  rhs <- gen val
  store a rhs
  return rhs

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

gen (S.Expr _ _ (S.Let a b c)) = case a of
  (S.Expr _ _ (S.Var a)) -> do
    i <- alloca double
    val <- gen b
    store i val
    assignVar a i
    gen c
  (S.Expr _ _ bad) -> throwError $ E.BadSpecialForm "Bad form in let" bad
gen ast = throwError . E.Default $ "Not implement yet: \n" ++ show ast


liftIOError :: ExceptT String IO a -> IO a
liftIOError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.AST] -> IO AST.Module
codegen mod fns = case newAST of
  Right newAST ->
    withContext $ \ctx ->
      liftIOError $ withModuleFromAST ctx newAST $ \m -> do
        llstr <- moduleLLVMAssembly m
        putStrLn llstr
        return newAST
  Left err     -> do
    print err
    return mod
  where newAST = runLLVMOrThrow mod (runExceptT $ mapM topLevel fns)
