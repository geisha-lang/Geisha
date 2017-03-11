module Main where

import Geisha.Parser

import Control.Monad.Trans
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

import Geisha.Codegen.Emit
import Geisha.Codegen.LLVM

initModule :: AST.Module
initModule = emptyModule "prelude"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modu line = do
  let res = readExpr line
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modu ex
      return $ Just ast


-- processFile :: String -> IO (Maybe)

main :: IO ()
main = runInputT defaultSettings (loop initModule)
  where   loop mod = do
            minput <- getInputLine "ready> "
            case minput of
              Nothing -> outputStrLn "Goodbye."
              Just input -> do
                modn <- liftIO $ process mod input
                case modn of
                  Just modn -> loop modn
                  Nothing -> loop mod
