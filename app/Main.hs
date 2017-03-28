module Main where

import Prelude hiding (product)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except

import System.Console.Haskeline
import System.Environment

-- import qualified Data.HashMap as M

import qualified LLVM.General.AST as AST

import Geisha.Parser

-- import Geisha.Codegen.Emit
import Geisha.AST
import Geisha.Codegen.LLVM

import Geisha.TypeInfer

initModule :: AST.Module
initModule = emptyModule "prelude"

-- process :: AST.Module -> String -> IO (Maybe AST.Module)
-- process modu src = case readExpr src of
--   Left err -> print err >> return Nothing
--   Right ex -> do
--     print ex
--     ast <- codegen modu ex
--     return $ Just ast


-- processFile :: String -> IO (Maybe AST.Module)
-- processFile fn = do
--   src <- readFile fn
--   let modu = emptyModule fn
--   process modu src

showType :: TypeEnv -> String -> IO ()
showType env src = case readExpr src of
  Left err -> print err
  Right ast -> do
    putStrLn . unlines $ map show ast
    case fmap fst . runInfer env . inferTop $ ast of
      Right ast -> putStrLn . unlines $ map (show . formType) ast
      Left err  -> print err

preludeEnv :: TypeEnv
preludeEnv = envList [ ("+", Forall ["a"] $ product (TVar "a") (TVar "a") `arrow` TVar "a")
                     , ("=", Forall ["a"] $ product (TVar "a") (TVar "a") `arrow` Void)
                     , ("==", Forall ["a"] $ product (TVar "a") (TVar "a") `arrow` typeBool)
                     ]

main :: IO ()
main = do
  args <- getArgs
  if null args then
    runInputT defaultSettings (loop initModule)
  else do
    -- processFile $ head args
    src <- readFile $ head args
    showType preludeEnv src
  where loop mod = do
          minput <- getInputLine "ready> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              -- modn <- liftIO $ process mod input
              -- case modn of
              --   Just modn -> loop modn
              --   Nothing -> loop mod
              liftIO $ showType preludeEnv input
              loop mod

