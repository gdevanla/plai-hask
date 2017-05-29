{-# LANGUAGE DeriveFunctor #-}
module Main where

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
        "-" -> SSub
        "*" -> SMul
        "/" -> SDiv
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




main :: IO ()
main = do
  putStrLn "main"
