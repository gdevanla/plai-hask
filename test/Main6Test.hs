module Main6Test where

import qualified Data.Map as M
import qualified Control.Exception as E
import Data.Typeable
import Control.Monad
import Test.Tasty.HUnit
import Test.Tasty
import Text.Parsec
import Chapter6

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

test_interpretC = testCaseInfo "test_interpretC_with_Env" $ do
  let expr = (AppC (Var "ttest") (NumC 3))
  return $ show $ interpretExprC expr emptyEnv allFuncCDefs



-- (test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
--                      mt-env
--                       (list (fdC 'const5 '_ (numC 5))))
--         15)

--   (test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
--                        mt-env
--                         (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
--           16)

--   (test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
--                        mt-env
--                         (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
--                                              (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
--           22)')')))')')')))'))))')'))))'))))'))))


const_5 = FuncDefC {name=Var "const5", arg=Var "_", body=(NumC 5)}
double_f = FuncDefC {name=Var "double", arg=Var "x", body=PlusC (IdC $ Var "x") (IdC $ Var "x")}
quadrapule = FuncDefC {
  name=Var "quadrapule",
  arg=Var "x",
  body=(AppC
        (Var "double") (AppC (Var "double") (IdC $ Var "x")))}
f1 = FuncDefC {name=Var "f1", arg=Var "x", body=AppC (Var "f2") (NumC 4)}
f2 = FuncDefC {name=Var "f2", arg=Var "y", body=(PlusC (IdC $ Var "x") (IdC $ Var "y"))}

functions_fixtures = M.fromList [
  (name const_5, const_5),
  (name double_f, double_f),
  (name quadrapule, quadrapule),
  (name f1, f1),
  (name f2, f2)]

interpreter_fixtures :: [(ExprC, Integer)]
interpreter_fixtures = [
  ((PlusC (NumC 10) (AppC (Var "const5") (NumC 10))),  15),
  ((PlusC (NumC 10) (AppC (Var "double") (PlusC (NumC 1) (NumC 2)))), 16),
  ((PlusC (NumC 10) (AppC (Var "quadrapule") (PlusC (NumC 1) (NumC 2)))), 22),
  ((AppC (Var "f1") (NumC 3)), 7)
  ]

interpret_and_test expr answer = do
  let actual_answer = (interpretExprC expr emptyEnv functions_fixtures)::Integer
  assertEqual "check answer" answer actual_answer

create_interpreter_test testNum (expr, answer) =
  testCase (show testNum) $ interpret_and_test expr answer

test_interpreter = testGroup "test_interpreter"
  [create_interpreter_test x y | (x,y) <- zip [0..] interpreter_fixtures]
