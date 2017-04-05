import Test.HUnit
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Geisha.AST
-- import Geisha.Parser
-- import Geisha.Pass.Flatten
import Geisha.TypeInfer


testSolve tl tr = case either throwError return . runExcept $ unifies tl tr of
  Left err -> print err
  Right subst -> do
    print $ eqNoLoc tl tr
    print subst
    print $ apply subst tl
    print $ apply subst tr

typel = arrow NoLoc (TProd NoLoc (TVar NoLoc "a") (TVar NoLoc "a")) (TVar NoLoc "a")
typer = arrow NoLoc (TProd NoLoc (TVar NoLoc "c") (TVar NoLoc "d")) (TVar NoLoc "e")

typeI = arrow NoLoc (TProd NoLoc (TCon NoLoc "Int") (TCon NoLoc "Int")) (TCon NoLoc "Int")

arrl = arrow NoLoc (TVar NoLoc "a") (TVar NoLoc "a")
arrr = arrow NoLoc (TVar NoLoc "cc") (TVar NoLoc "dd")

arrI = arrow NoLoc (TCon NoLoc "Int") (TCon NoLoc "Int")

typeSr = TVar NoLoc "aa"
typeSl = TVar NoLoc "bb"

main = testSolve typel typer
-- main = runTestTT $ TestList [ flattenTest ]

