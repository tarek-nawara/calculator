-------------------------------------------------
-- |
-- Module : Calculator
-- Parsing arithmatic expression and evaluate it
-- author Tarek Nawara
--
-------------------------------------------------
module Calculator
  ( evalAll
  , defaultVars
  ) where

import Control.Monad.State
import qualified Data.Map as M
import Interp (Calc, Expr(..), interStatement, StoredV)
import Lexical (parseInput)
import Text.Parsec

-- Default Constant Values to start the environment with
defaultVars :: M.Map String StoredV
defaultVars = M.fromList [("pi", Left 3.141), ("MAX_INT", Left 1000)]

-- | Evaluate individual statements in the program
evalStatement :: String -> Calc ()
evalStatement s =
  case ret of
    Left e -> liftIO $ putStrLn $ "error: " ++ show e
    Right n -> interStatement n
  where
    ret = parse parseInput "" s

-- | Evaluate the entire program
evalAll :: Calc ()
evalAll = liftIO getContents >>= mapM_ evalStatement . lines
