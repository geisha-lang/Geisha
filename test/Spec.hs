import Test.HUnit
import Data.Monoid
import Control.Monad
import Control.Monad.State

import Geisha.AST
import Geisha.Parser
import Geisha.Pass.Flatten

testWithParse desc expr assertion = TestLabel desc $ TestCase $ case readExpr expr of
    Left err  -> assertFailure $ "While parsing:\n" ++ expr ++ "\n" ++ show err
    Right res -> assertion res

testParse :: String -> String -> AST -> Test
testParse desc expr ast = testWithParse desc expr (\e -> assertEqual desc e ast)
-- testParse desc expr ast = TestLabel desc $ TestCase $ case readExpr expr of
--     Left err  -> assertFailure $ "While parsing:\n" ++ expr ++ "\n" ++ show err
--     Right res -> assertEqual desc ast res

testFlatten desc expr flattenRes = testWithParse desc expr assertFlatten
    where assertFlatten ast = assertEqual desc flattenRes (evalState (flatten ast (Just $ Var "test")) 0)

-- parseArithExpr = testParse
--                 "Parse arithmetic expressions to AST"
--                 "1 * 2 + 3 * 4"
--                 (BinExpr "+"
--                     (BinExpr "*"
--                         (Integer 1)
--                         (Integer 2))
--                     (BinExpr "*"
--                         (Integer 3)
--                         (Integer 4)))

-- parseLambda = testParse
--                 "Parse lambda function"
--                 "(a , b) -> {\n a + b \n}"
--                 (Function $ FunctionNode [ Ident "a", Ident "b" ]
--                                          $ Block [ BinExpr "+"
--                                                     (Ident "a")
--                                                     (Ident "b")])

-- parseList = testParse
--                 "Parse list of expr"
--                 "[a, b, 1]"
--               $ List [ Ident "a", Ident "b", Integer 1 ]

-- parseFunCall = testParse
--                 "Parse function call"
--                 "a + fuck(1 + 2, 3) + shit ( a,b )"
--                 (BinExpr "+"
--                     (BinExpr "+"
--                         (Ident "a")
--                         (FunCall (Ident "fuck")
--                                  [BinExpr "+" (Integer 1) (Integer 2), Integer 3]))
--                     (FunCall (Ident "shit") [Ident "a", Ident "b"]))

flattenTest = testFlatten
                "Flatten arithmetic expression"
                "1 * 2 + 3 * 4"
                (Var "test",
                 [ Assign (Var "tmp.0") (BinInstr "*" (Int 1) (Int 2)),
                   Assign (Var "tmp.1") (BinInstr "*" (Int 3) (Int 4)),
                   Assign (Var "test") (BinInstr "+" (Var "tmp.0") (Var "tmp.1")) ],
                 [ "tmp.0", "tmp.1" ])


main = runTestTT $ TestList [ flattenTest ]
