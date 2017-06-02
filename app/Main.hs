{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Prim

data Op = SAdd | SSub | SMul | SDiv
  deriving Show

data SExpr = Atom Integer
  | Symbol Op
  | SIdent String
  | SExprList [SExpr]
  deriving Show


plaiDef :: LanguageDef st
plaiDef = emptyDef
  {
    commentStart   = "#|"
  , commentEnd     = "|#"  -- no support for s-expr comment
  , commentLine    = ";"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = opLetter plaiDef
  , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames= []
  , reservedNames  = []
  , caseSensitive  = True
  }


lexer  = makeTokenParser plaiDef

allowedSymbolsInFirstElement:: Parser SExpr
allowedSymbolsInFirstElement =  do
  -- c <- (symbol lexer) "+" <|> (symbol lexer) "-"
  let p = map (symbol lexer) ["+", "-", "*", "/"]
  c <- foldl1 (<|>) p
  let s = case c of
        "+" -> SAdd
--        "-" -> SSub
        "*" -> SMul
--        "/" -> SDiv
  return $ Symbol s

parseIdentifier = do
  var <- (identifier lexer)
  return $ SIdent var

parseAtom = do
  i <- (integer lexer)
  return $ Atom i

-- parseSubExpr' = do
--   e <- (many parseExpr)
--   return $ SExprList e

parseSubExpr =
  allowedSymbolsInFirstElement <|>
  parseIdentifier <|>
  parseAtom <|>
  parseExpr

parseExpr = do
  e <- (parens lexer) (many parseSubExpr)
  return $ SExprList e


---------------

data ArithC =
  NumC Integer
  | PlusC ArithC ArithC
  | MultC ArithC ArithC
--  | SubC ArithC ArithC
  | DivC ArithC ArithC
    deriving (Show)


parseL (Atom a) = NumC a
parseL (SExprList (x:y:z:cs)) = case x of
  (Symbol a) -> case a of
                  SAdd -> (PlusC (parseL y) (parseL z))
                  SMul -> (MultC (parseL y) (parseL z))
                  --SSub -> (SubC (parseL y) (parseL z))
                  SDiv -> (DivC (parseL y) (parseL z))

interpretL (PlusC x y) = (+) (interpretL x) (interpretL y)
interpretL (MultC x y) = (*) (interpretL x) (interpretL y)
--interpretL (SubC x y) = (-) (interpretL x) (interpretL y)
interpretL (DivC x y) = (div) (interpretL x) (interpretL y)
interpretL (NumC a) = a

main :: IO ()
main = do
  putStrLn "Enter the expression here"
  expr <- getLine
  unless (expr == "q") $ do
    let pexpr = parse parseExpr "" expr
    case pexpr of
      Right x -> putStrLn $ show $ interpretL $ parseL x
      Left x -> putStrLn $ "There was an error" ++ show x
    main
