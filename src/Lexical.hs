----------------------------------------------
-- |
-- Module Lexical
-- Holding the logic for parsing the tokens and
-- building the parse tree
-- author Tarek Nawara
--
-------------------------------------------------
module Lexical
  ( parsePrint
  , parseInput
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Interp (Expr(..), Statement(..), FunBody(..))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

-- | Definition of the lexer
lexer :: TokenParser ()
lexer =
  makeTokenParser
    (javaStyle {opStart = oneOf "+-*/%", opLetter = oneOf "+-*/%"})

-- * Parsing Tokens
-- | Parsing a number and return its expression equivalent representation
parseNumber :: Parser Expr
parseNumber = do
  val <- naturalOrFloat lexer
  case val of
    Left i -> return (Const (fromIntegral i))
    Right n -> return (Const n)

-- | Parsing function invocation
parseFunInvoc :: Parser Expr
parseFunInvoc = do
  ident <- identifier lexer
  expr <- parens lexer parseExpr
  return (FunInvoc ident expr)

-- | Parsing any token
parseTerm :: Parser Expr
parseTerm =
  parens lexer parseExpr <|> parseNumber <|> try parseFunInvoc <|>
  fmap Id (identifier lexer)

-- | Function to parse the tokens and return
--   the equivalent parse tree of expressions
parseExpr :: Parser Expr
parseExpr =
  buildExpressionParser
    [ [ Prefix (reservedOp lexer "-" >> return Neg)
      , Prefix (reservedOp lexer "+" >> return id)
      ]
    , [ Infix (reservedOp lexer "*" >> return Mul) AssocLeft
      , Infix (reservedOp lexer "/" >> return Div) AssocLeft
      , Infix (reservedOp lexer "%" >> return Mod) AssocLeft
      , Infix (reservedOp lexer "+" >> return Add) AssocLeft
      , Infix (reservedOp lexer "-" >> return Sub) AssocLeft
      ]
    ]
    parseTerm

-- * Parsing Statements
-- | Parsing print statement
parsePrint :: Parser Statement
parsePrint = do
  reserved lexer "print"
  expr <- parseExpr
  return (PrintS expr)

-- | Parsing Assignment Statement
parseAssign :: Parser Statement
parseAssign = do
  reserved lexer "let"
  ident <- identifier lexer
  reservedOp lexer "="
  expr <- parseExpr
  return (AssignS ident expr)

-- | Parsing Definition of function
parseFunDef :: Parser Statement
parseFunDef = do
  reserved lexer "def"
  ident <- identifier lexer
  arg <- parens lexer $ identifier lexer
  reservedOp lexer "="
  expr <- parseExpr
  return (FunDef ident (FunBody arg expr))

-- * Parsing the program
parseInput :: Parser Statement
parseInput = do
  whiteSpace lexer
  s <- parsePrint <|> parseAssign <|> parseFunDef
  eof
  return s
