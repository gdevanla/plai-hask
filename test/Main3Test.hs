module Main3Test where

import qualified Control.Exception as E
import Data.Typeable
import Control.Monad
import Test.Tasty.HUnit
import Test.Tasty
import Text.Parsec
import Chapter5

import Data.List

fixtures = [
  ("(+ 10 10)", Right (SExprList [Symbols SAdd,Atom 10,Atom 10]))
  ,("(-10)", Right (SExprList [Atom (-10)]))
  ,("(+ 10 (+ 20 30) -40)",
     (Right (SExprList
                         [Symbols SAdd,
                          Atom 10,
                          SExprList [Symbols SAdd,
                                     Atom 20,
                                     Atom 30],
                           Atom (-40)])))
  ,("(x 10 a)", (Right (SExprList
                       [Symbols (Var "x")
                       ,Atom 10
                       ,Symbols (Var "a")])))]

parseAndTest expr expected_pexpr = do
  let pexpr = parse parseExpr "" expr
    in
    assertEqual "parseAndTest" expected_pexpr pexpr

create_test testNum (expr, expected_pexpr) =
  testCase (show testNum) $ parseAndTest expr expected_pexpr


test_SExpressions = testGroup "test_SExpressions"
  [create_test x y | (x,y) <- zip [0..] fixtures]

test_pprint = testCaseInfo "test_pprint" $ do
  let x = map (
        pprintSExpr .
        (either (\_ -> (Atom 10)) id) .
        snd) $ fixtures
  return $ intercalate "\n" x


test_subst = testCaseInfo "test_subst" $ do
  let what = (AppC (Var "fn") (NumC 3))
      for = Var "x"
      inexprs = [PlusC (IdC $ Var "x") (IdC $ Var "y")
                , IdC $ Var "x"
                , AppC (Var "fn") (PlusC (IdC $ Var "y") (IdC $ Var "x"))]
  return $ intercalate "\n" $ map (show . (subst what for)) $ inexprs

test_interpretC = testCaseInfo "test_interpretC" $ do
  let expr = (AppC (Var "test") (NumC 3))
  return $ show $ interpretExprC expr $ getFDef










































-- test_parseExpr1 = testCase "parseExpr1" $ do
--   let expr = "(+ 10 10)"
--       pexr = parse parseExpr "" expr
--       expected = Right (SExprList [Symbols SAdd,Atom 10,Atom 10])
--     in
--       assertEqual "Simple Expr" expected pexr

-- test_parseExpr2 = testCaseInfo "parseExpr2" $ do
--   let expr = "(-10)"
--       pexr = parse parseExpr "" expr
--     in
--     return $ show pexr

-- data MyException = MyException String
--     deriving (Show, Typeable)
-- instance E.Exception MyException

-- test_parseExpr3 = testCase "parseExpr3" $ do
--   let expr = "(+ 10 (+ 20 30) -40)"
--       pexr = parse parseExpr "" expr
--       expected = (Right (SExprList
--                          [Symbols SAdd,
--                           Atom 10,
--                           SExprList [Symbols SAdd,
--                                      Atom 20,
--                                      Atom 30],
--                            Atom (-40)]))
--     in
--     assertEqual "Nested + and Unary expr" expected pexr
    --return $ ""
    --E.throwIO (MyException "test_message")


    --putStrLn $ show pexr
    --return $ show pexr


-- test_Main3:: TestTree
-- test_Main3 = testGroup "Tests" [
--   test_parseExpr1
--   test_parseExpr2
--   --, testMain2
--   --testMain2
--   ]
