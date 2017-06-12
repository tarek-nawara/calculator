----------------------------------------------------
-- |
-- Module Interp
-- Holding the logic for evaluating the parse tree
-- author Tarek Nawara
--
---------------------------------------------------
module Interp
  ( Expr(..)
  , Statement(..)
  , FunBody(..)
  , Calc
  , StoredV
  , interStatement
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

-- | Representation of program expressions
data Expr
  = Const Double
  | Id String
  | Add Expr
        Expr
  | Sub Expr
        Expr
  | Mul Expr
        Expr
  | Div Expr
        Expr
  | Mod Expr
        Expr
  | Neg Expr
  | FunInvoc String
             Expr
  deriving (Show)

-- | Representation of function body
data FunBody =
  FunBody String
          Expr
  deriving (Show)

-- | Representation of Statements
data Statement
  = PrintS Expr
  | AssignS String
            Expr
  | FunDef String
           FunBody
  deriving (Show)

-- | Sotred Value in the environment
type StoredV = Either Double FunBody
-- | Environment
type Calc a = StateT (M.Map String StoredV) IO a

-- | interpreting the expressions
--   result will be the StateT that will
--   contains the context of the evaluation
interExpr :: Expr -> Calc Double
interExpr (Const n) = return n
interExpr (Add e1 e2) = do
  v1 <- interExpr e1
  v2 <- interExpr e2
  return (v1 + v2)
interExpr (Sub e1 e2) = do
  v1 <- interExpr e1
  v2 <- interExpr e2
  return (v1 - v2)
interExpr (Mul e1 e2) = do
  v1 <- interExpr e1
  v2 <- interExpr e2
  return (v1 * v2)
interExpr (Div e1 e2) = do
  v1 <- interExpr e1
  v2 <- interExpr e2
  return (v1 / v2)
interExpr (Mod e1 e2) = do
  v1 <- interExpr e1
  v2 <- interExpr e2
  return (fromIntegral (floor v1 `mod` floor v2))
interExpr (Neg e) = do
  v <- interExpr e
  return (negate v)
interExpr (Id i) = do
  varmap <- get
  case M.lookup i varmap of
    Nothing -> fail ("Unknown variable: " ++ i)
    Just (Left n) -> return n
    Just (Right n) -> fail ("You must call function: " ++ i)

-- | Interpreting Statements
--   each statement when interpreted should evaluate
--   to IO action and then will yield to State of ()
interStatement :: Statement -> Calc ()
interStatement (PrintS expr) = do
  n <- interExpr expr
  liftIO (print n)
interStatement (AssignS id expr) = do
  n <- interExpr expr
  modify (M.insert id (Left n))
