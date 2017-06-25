module Main where

import Chapter5
import Text.Parsec
import Control.Monad

main :: IO ()
main = do
  putStrLn "Enter the expression here"
  expr <- getLine
  unless (expr == "q") $ do
    let pexpr = parse parseExpr "" expr
    case pexpr of
      Right x -> putStrLn $ show $ interpretExprC (desugar $ parseArithS x) $ allFuncCDefs
      Left x -> putStrLn $ "There was an error" ++ show x
    main
