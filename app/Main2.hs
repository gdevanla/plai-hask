{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Prim as P

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
        "-" -> SSub
        "*" -> SMul
        "/" -> SDiv
  return $ Symbol s

unaryAtom:: Parser SExpr
unaryAtom = P.try $ do
  c <- char '-'
  i <- (natural lexer)
  return $ Atom (-i)

parseIdentifier = do
  var <- (identifier lexer)
  return $ SIdent var

parseAtom = do
  i <- (natural lexer)
  return $ Atom i

-- parseSubExpr' = do
--   e <- (many parseExpr)
--   return $ SExprList e

parseSubExpr =
  unaryAtom <|>
  allowedSymbolsInFirstElement <|>
  parseAtom <|>
  parseIdentifier <|>
  parseExpr

parseExpr = do
  e <- (parens lexer) (many parseSubExpr)
  return $ SExprList e


---------------

data ArithC =
  NumC Integer
  | PlusC ArithC ArithC
  | MultC ArithC ArithC
    deriving (Show)

data ArithS =
  NumS Integer
  | PlusS ArithS ArithS
  | BMinusS ArithS ArithS
  | MultS ArithS ArithS


desugar :: ArithS -> ArithC
desugar (NumS a) = NumC a
desugar (PlusS x y) = PlusC (desugar x) (desugar y)
desugar (MultS x y) = MultC (desugar x) (desugar y)
desugar (BMinusS x y) = PlusC (desugar x) (MultC (NumC (-1)) (desugar y))

parseArithC (Atom a) = NumC a
parseArithC (SExprList (x:y:z:cs)) = case x of
  (Symbol a) -> case a of
                  SAdd -> (PlusC (parseArithC y) (parseArithC z))
                  SMul -> (MultC (parseArithC y) (parseArithC z))

parseArithS (Atom a) = NumS a
parseArithS (SExprList (x:y:z:cs)) = case x of
  (Symbol a) -> case a of
                  SAdd -> PlusS (parseArithS y) (parseArithS z)
                  SMul -> MultS (parseArithS y) (parseArithS z)
                  SSub -> BMinusS (parseArithS y) (parseArithS z)
parseArithS (SExprList (Atom x:[])) = NumS x


interpretArithC (PlusC x y) = (+) (interpretArithC x) (interpretArithC y)
interpretArithC (MultC x y) = (*) (interpretArithC x) (interpretArithC y)
interpretArithC (NumC a) = a

main :: IO ()
main = do
  putStrLn "Enter the expression here"
  expr <- getLine
  unless (expr == "q") $ do
    let pexpr = parse parseExpr "" expr
    case pexpr of
      Right x -> putStrLn $ show $ interpretArithC $ desugar $ parseArithS x
      Left x -> putStrLn $ "There was an error" ++ show x
    main
