module Geisha.TypeInfer.Env where

import qualified Data.HashMap.Strict as M

import Geisha.AST
import Geisha.AST.PrettyPrint

newtype TypeEnv = TypeEnv (M.HashMap Name Scheme)
                deriving (Eq)

instance Show TypeEnv where
  show (TypeEnv m) = unlines $ map (\(n, t) -> n ++ " :: " ++ show t) $ M.toList m

emptyEnv = TypeEnv M.empty
envList = TypeEnv . M.fromList

extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (n, s) = TypeEnv $ M.insert n s env

remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) v = TypeEnv $ M.delete v env

