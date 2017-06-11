--------------------------------------
-- |
-- Basic Calculator
-- Read arithmatic expression from console
-- and evaluate it
-- author Tarek Nawara
--
-------------------------------------
module Main
  ( module Main
  ) where

import Calculator(calculate)

-- | Main function
--   Reads expression from console and produce
--   the result of evaluating it 
main :: IO ()
main = interact (unlines . map calculate  . lines)
