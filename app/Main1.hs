{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Char
import Debug.Trace

newtype Parser a = P (String -> [(a, String)])
  deriving (Functor)

--- monad instance for parser

returnP a =  P $ \cs -> [(a, cs)]

-- bad version
-- bindP p f = P $ \cs -> let (x, cs') = head $ doParse p cs
--                            (y, cs'') = head $ doParse (f x) cs'
--                            in
--                          [(y, cs'')]

bindP p f = P $ \cs -> [
  (y, cs'') |
    (x, cs') <- doParse p cs,
    (y, cs'') <- doParse (f x) cs']

instance Applicative Parser where
  pure = return
  (<*>) p1 p2 = undefined

instance Monad Parser where
  return = returnP
  (>>=) = bindP

  ----------------------------------------------------------------

oneChar = P $ \s -> case s of
  [] -> []
  c:cs -> [(c, cs)]


twoChar :: Parser t1 -> Parser t -> Parser (t1, t)
twoChar p1 p2 = P $ f where
  f s = let (a, x) = head $ doParse p1 s
            (b, y) = head $ doParse p2 x
        in
          [((a, b), y)]

pairP :: Monad m => m t1 -> m t -> m (t1, t)
pairP p1 p2 = p1 >>= (\x -> (p2 >>= (\y -> return (x, y))))

failP = P $ const []

satP ::  (Char -> Bool) -> Parser Char
satP p = do
  c <- oneChar
  -- trace (show c)  (return c)
  if p c then return c else failP

lowerCaseP = satP isAsciiLower

alphaChar = satP isAlpha
digitChar = satP isDigit

char c = (satP (== c))

digitInt = do
  c <- digitChar
  return ((read [c])::Int)


strP :: String -> Parser String
strP (c:cs) = do
  x <- (satP (==c))
  y <- strP cs
  return (x:y)
strP "" = return ""

chooseP :: Parser a -> Parser a -> Parser a
chooseP p1 p2 = P (\cs -> (doParse p1 cs) ++ (doParse p2 cs))

grabn :: Int -> Parser String
grabn n
  | n <= 0 = return ""
  | otherwise = do
      c <- oneChar
      cs <- grabn (n - 1)
      return (c:cs)


intOp = plus `chooseP` minus `chooseP` times `chooseP` divide
  where
    plus = char '+' >> return (+)
    minus = char '-' >> return (-)
    times = char '*' >> return (*)
    divide = char '/' >> return div

calc = do x <- digitInt
          y <- intOp
          z <- digitInt
          return $ x `y` z



manyP :: Parser a -> Parser [a]
manyP p = many1 `chooseP` many0
  where
    many0 =  return []
    many1 = do
      x <- p
      xs <- manyP p
      return (x:xs)


(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case doParse  (p1 `chooseP` p2) cs of
  [] -> []
  x:_ -> [x]


mmanyP :: Parser a -> Parser [a]
mmanyP p = many1 <|> many0
  where
    many0 =  return []
    many1 = do
      x <- p
      xs <- mmanyP p
      return (x:xs)


---- tests ----


doParse :: Parser t -> String -> [(t, String)]
doParse (P p) i = p i

testOneChar = do
  let c = doParse oneChar "test"
  putStrLn $ show $ c

main :: IO ()
main = do
  putStrLn "main"
  testOneChar
