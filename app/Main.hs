module Main where

import Geisha.Parser

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment

import qualified LLVM.General.AST as AST

import Geisha.Codegen.Emit
import Geisha.Codegen.LLVM

initModule :: AST.Module
initModule = emptyModule "prelude"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modu src = case readExpr src of
  Left err -> print err >> return Nothing
  Right ex -> do
    print ex
    ast <- codegen modu ex
    return $ Just ast


processFile :: String -> IO (Maybe AST.Module)
processFile fn = do
  src <- readFile fn
  let modu = emptyModule fn
  process modu src

main :: IO ()
main = do
  args <- getArgs
  if null args then
    runInputT defaultSettings (loop initModule)
  else do
    processFile $ head args
    return ()
  where loop mod = do
          minput <- getInputLine "ready> "
          case minput of
            Nothing -> outputStrLn "Goodbye."
            Just input -> do
              modn <- liftIO $ process mod input
              case modn of
                Just modn -> loop modn
                Nothing -> loop mod
