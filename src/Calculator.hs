-------------------------------------------------
-- |
-- Module : Calculator
-- Parsing arithmatic expression and evaluate it
-- author Tarek Nawara
--
-------------------------------------------------
module Calculator
  ( calculate
  ) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

doubleMod :: Double -> Double -> Double
doubleMod top bottom = fromInteger $ floor top `mod` floor bottom

lexer :: TokenParser ()
lexer =
  makeTokenParser
    (javaStyle {opStart = oneOf "+-*/%", opLetter = oneOf "+-*/%"})

parseExpression :: Parser Double
parseExpression =
  buildExpressionParser
    [ [ Prefix (reservedOp lexer "-" >> return negate)
      , Prefix (reservedOp lexer "+" >> return id)
      ]
    , [ Infix (reservedOp lexer "*" >> return (*)) AssocLeft
      , Infix (reservedOp lexer "/" >> return (/)) AssocLeft
      , Infix (reservedOp lexer "%" >> return doubleMod) AssocLeft
      , Infix (reservedOp lexer "+" >> return (+)) AssocLeft
      , Infix (reservedOp lexer "-" >> return (-)) AssocLeft
      ]
    ]
    parseTerm

parseTerm :: Parser Double
parseTerm = parens lexer parseExpression <|> parseNumber

parseInput :: Parser Double
parseInput = do
  whiteSpace lexer
  res <- parseExpression
  eof
  return res

parseNumber :: Parser Double
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return (fromIntegral i)
    Right n -> return n

calculate :: String -> String
calculate s =
  case ret of
    Left e -> "error: " ++ show e
    Right n -> "answer: " ++ show n
  where
    ret = parse parseInput "" s
