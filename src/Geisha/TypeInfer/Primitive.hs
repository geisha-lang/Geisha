module Geisha.TypeInfer.Primitive (preludeEnv) where


import Geisha.TypeInfer.Env as T
import Geisha.AST as AST

import Geisha.TypeInfer as TI

arr = arrow NoLoc
pro = TI.product NoLoc

var = TVar NoLoc

add = ("+", Forall [] $ pro (TCon NoLoc "Int") (TCon NoLoc "Int") `arr` TCon NoLoc "Int")

preludeEnv :: TypeEnv
preludeEnv = envList [ ("+", Forall ["a"] $ pro (var "a") (var "a") `arr` var "a")
                       -- add
                     , ("add", Forall ["a"] $ var "a" `arr` (var "a" `arr` var "a"))
                     , ("cons", Forall ["a", "b"] $ pro (var "a") (var "b") `arr` pro (var "a") (var "b"))
                     , ("=", Forall ["a"] $ pro (var "a") (var "a") `arr` Void NoLoc)
                     , ("==", Forall ["a"] $ pro (var "a") (var "a") `arr` typeBool NoLoc)
                     ]
