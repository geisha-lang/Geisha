module Geisha.Compile (
  CompileM,
  CompileState,
  SyntaxModule,
  compileP,
  runCompileM
) where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Trans

import qualified Geisha.AST                 as AST
import           Geisha.AST.PrettyPrint
import           Geisha.Error
import qualified Geisha.TypeInfer.Env       as T



-- | Compiling chains
import qualified Geisha.Parser              as P
import qualified Geisha.TypeInfer           as TI

import           Geisha.TypeInfer.Primitive

type CompileM = ExceptT CompileErr (StateT CompileState IO)

data CompileState = CompileState {
  _fname   :: Maybe FilePath,
  _imports :: [FilePath],
  _exports :: [AST.Name],
  _source  :: Maybe String,
  _tyEnv   :: T.TypeEnv,
  _AST     :: SyntaxModule
} deriving (Show)

newtype SyntaxModule = SyntaxModule {
  _decls :: [AST.Syntax]
} deriving (Show)


-- mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
-- flip runStateT initCompileState ::
--   StateT CompileState IO (Either CompileErr a) -> IO ((Either CompileErr a), CompileState)
-- runExceptT :: ExceptT CompileErr (StateT CompileState IO) a -> StateT CompileState IO (Either CompileErr a)

runCompileM :: CompileM a -> IO (Either CompileErr (a, CompileState))
runCompileM c = do
  (res, cs) <- flip runStateT initCompileState $ runExceptT c
  return $ case res of
    Left err -> throwError err
    Right a  -> return (a, cs)

emptyModule = SyntaxModule []

initCompileState = CompileState Nothing [] [] Nothing preludeEnv emptyModule


compileP :: FilePath -> CompileM SyntaxModule
compileP = openSrcFile
       >=> parseP
       >=> inferP

openSrcFile :: FilePath -> CompileM String
openSrcFile fpath = lift $ do
  src <- liftIO $ readFile fpath
  modify $ \s -> s {
    _fname = Just fpath,
    _source = Just src
  }
  return src

parseP :: String -> CompileM SyntaxModule
parseP src = do
  mod <- SyntaxModule <$> P.readSource src
  lift . modify $ \s -> s { _AST = mod }
  return mod

inferP :: SyntaxModule -> CompileM SyntaxModule
inferP (SyntaxModule decls) = do
  env <- gets _tyEnv
  ((ast, env), cs) <- liftTypeError $ TI.runInfer env . TI.inferTop $ decls
  let modu = SyntaxModule ast
  modify $ \s -> s { _AST = modu }
  liftIO . putStrLn . unlines . map show $ ast
  -- liftIO . putStrLn . unlines . map (show . AST.syntaxType) $ ast
  liftIO . print $ cs
  liftIO $ print env
  return modu
