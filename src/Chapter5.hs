module Chapter5 where

import Control.Monad
import Text.Parsec.Prim
import Text.Parsec.Token
import Text.Parsec
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Prim as P


data Symbol = SAdd | SSub | SMul | SDiv | Var String
  deriving (Show, Eq)

data SExpr = Atom Integer
  | Symbols Symbol
  | SIdent String
  | SExprList [SExpr]
  deriving (Show, Eq)

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
  return $ Symbols $ s

unaryAtom:: Parser SExpr
unaryAtom = P.try $ do
  c <- char '-'
  i <- (natural lexer)
  return $ Atom (-i)

parseIdentifier = do
  var <- (identifier lexer)
  return $ Symbols $ Var var

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

data ExprC =
  NumC Integer
  | IdC Symbol
  | PlusC ExprC ExprC
  | MultC ExprC ExprC
  | AppC Symbol ExprC
    deriving (Show)

data ArithS =
  NumS Integer
  | IdS Symbol
  | PlusS ArithS ArithS
  | BMinusS ArithS ArithS
  | MultS ArithS ArithS
  | AppS Symbol ArithS
  deriving Show


data FuncDefC = FuncDefC {name::Symbol, arg::Symbol, body::ExprC} deriving Show
--data Appl = Appl {fun:: Symbol, args::ExprC} deriving Show
--data IdC = IdC Symbol

desugar :: ArithS -> ExprC
desugar (NumS a) = NumC a
desugar (PlusS x y) = PlusC (desugar x) (desugar y)
desugar (MultS x y) = MultC (desugar x) (desugar y)
desugar (BMinusS x y) = PlusC (desugar x) (MultC (NumC (-1)) (desugar y))
desugar (IdS s) = IdC s
desugar (AppS s a) = AppC s (desugar a)

parseExprC (Atom a) = NumC a
parseExprC (SExprList (x:y:z:cs)) = case x of
  (Symbols a) -> case a of
                  SAdd -> (PlusC (parseExprC y) (parseExprC z))
                  SMul -> (MultC (parseExprC y) (parseExprC z))


--parseArithS (SExprList (Atom x:[])) = NumS x

parseArithS :: SExpr -> ArithS
parseArithS (Atom a) = NumS a
parseArithS (SIdent s)= IdS (Var s)
parseArithS (SExprList (x:xs)) = case x of
  (Symbols a) -> case a of
                  SAdd -> mathOp PlusS xs
                  SMul -> mathOp MultS xs
                  SSub -> mathOp BMinusS xs
                  Var a -> appOp (AppS (Var a)) xs
                  --Atom x -> NumS x
  where
    mathOp f (x:y:[]) = f (parseArithS x) (parseArithS y)
    appOp f (x:[]) = f (parseArithS x)

interpretExprC (PlusC x y) = (+) (interpretExprC x) (interpretExprC y)
interpretExprC (MultC x y) = (*) (interpretExprC x) (interpretExprC y)
interpretExprC (NumC a) = a