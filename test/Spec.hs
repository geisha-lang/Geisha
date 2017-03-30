import Test.HUnit
import Data.Monoid
import Control.Monad
import Control.Monad.State

import Geisha.AST
import Geisha.Parser
import Geisha.Pass.Flatten


main = runTestTT $ TestList [ flattenTest ]
