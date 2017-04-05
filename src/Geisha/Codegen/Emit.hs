module Geisha.Codegen.Emit where

import Control.Applicative
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
import Data.Maybe
import qualified Data.Map as M

import Geisha.Codegen.LLVM
import Geisha.Codegen.Primitive
import Geisha.AST as S
import qualified Geisha.Error as E


liftThrows :: E.ThrowsCompileErr a -> ExceptT E.CompileErr LLVM a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
-- liftCompileError :: Monad m => ExceptT E.CompileErr Codegen -> m a

extractIdent :: Form -> Maybe String
extractIdent (Expr (ASTNode _ _ (Var n))) = Just n
extractIdent bad                          = Nothing

topLevel :: Form -> ExceptT E.CompileErr LLVM ()
topLevel (Decl (ASTNode _ _ (Define name (Expr (ASTNode _ _ (S.Function (Lambda args' body))))))) = do
  let args = fromJust $ mapM extractIdent args'
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

gen :: S.Form -> CodegenErrorT AST.Operand
gen (S.Expr (S.ASTNode _ _ exp)) = genExp exp
  where
    genExp (S.Lit lit) = return . cons $ case lit of
      LInt i      -> C.Int 32 i
      LFloat f    -> C.Float $ F.Double f
      LBool True  -> C.Float $ F.Double 1.0
      LBool False -> C.Float $ F.Double 0.0
      -- return . cons . C.Float $ F.Double f
    -- gen (S.Integer i)) = return . cons $ C.Int 32 i
    genExp (S.Var v) = getVar v >>= load
    genExp (S.Apply op [lhs, rhs])
      | fromMaybe False $ (`M.member` binops) <$> extractIdent op = do
          let f = fromJust $ (M.!) binops <$> extractIdent op
          clhs <- gen lhs
          crhs <- gen rhs
          f clhs crhs
    genExp (S.Apply fn args) = do
      largs <- mapM gen args
      call (externf integer $ AST.Name fun) largs
      where (S.Var fun)) = fn

    -- gen (S.BinExpr "=" (S.Var var)) val)) = do
    --   a <- getVar var
    --   rhs <- gen val
    --   store a rhs
    --   return rhs

    -- gen (S.BinExpr op l r)) =
    --   case M.lookup op binops of
    --     Just f -> do
    --       clhs <- gen l
    --       crhs <- gen r
    --       f clhs crhs
    --     Nothing -> throwError . E.Default $ "No such operator '" ++ op ++ "'"
    genExp (S.Apply (S.Var "=") [S.Expr (S.ASTNode _ _ (S.Var var), val])) = do
      a <- getVar var
      rhs <- gen val
      store a rhs
      return rhs

    genExp (S.Block exps)) = loop exps
      where loop [e] = gen e
            loop (e:es) = do
              gen e
              loop es

    genExp (S.If cond tr fl)) = do
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

    genExp (S.Let a b c)) = case a of
      (S.Expr (S.ASTNode _ _ (S.Var a))) -> do
        i <- alloca double
        val <- gen b
        store i val
        assignVar a i
        gen c
      (S.Expr (S.ASTNode _ _ bad)) -> throwError $ E.BadSpecialForm "Bad form in let" bad
    genExp ast = throwError . E.Default $ "Not implement yet: \n" ++ show ast


liftIOError :: ExceptT String IO a -> IO a
liftIOError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Form] -> IO AST.Module
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
