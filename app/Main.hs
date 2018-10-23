module Main where

import Prelude hiding (product)
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Except

import System.Console.Haskeline
import System.Environment

-- import qualified Data.HashMap as M

import qualified LLVM.AST as AST

import Geisha.Parser

-- import Geisha.Codegen.Emit
import Geisha.AST
import Geisha.Codegen.LLVM

import Geisha.TypeInfer
import Geisha.TypeInfer.Env

import Geisha.Compile

initModule :: AST.Module
initModule = emptyModule "prelude"

-- process :: AST.Module -> String -> IO (Maybe AST.Module)
-- process modu src = case readSource src of
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

main :: IO ()
main = do
  args <- getArgs
  if null args then return ()
    -- runInputT defaultSettings (loop initModule)
  else do
    res <- runCompileM . compileP $ head args
    case res of
      Left err -> print err
      Right r  -> return ()
  -- where loop mod = do
  --         minput <- getInputLine "ready> "
  --         case minput of
  --           Nothing -> outputStrLn "Goodbye."
  --           Just input -> do
  --             liftIO $ showType preludeEnv input
  --             loop mod

