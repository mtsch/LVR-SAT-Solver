{-# OPTIONS_GHC -Wall #-}
module Types ( Literal (..)
             , Clause
             , Formula
             , Value (..)
             , Valuation
             , getVariable
             , getValue
             , getLitValue
             , isUnassigned
             , setValue
             , initValuation
             , getValuation
             ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

data Literal = Lit Int
             | Not Int

instance Show Literal where
    show (Lit l) = "+" ++ show l
    show (Not l) = "-" ++ show l

type Clause = [Literal]

type Formula = [Clause]

-- True, False or Unassigned.
data Value = TRUE | FALSE | NA
             deriving (Show, Eq)

-- Collection of variables and their values.
type Valuation = IntMap Value

-- Get the variable name from literal
getVariable :: Literal -> Int
getVariable (Lit i) = i
getVariable (Not i) = i

-- Get value of variable from valuation.
getValue :: Valuation -> Int -> Value
getValue val i = case IntMap.lookup i val of
                   Just v  -> v
                   Nothing -> NA

getLitValue :: Valuation -> Literal -> Value
getLitValue val = getValue val . getVariable

-- Check if variable is unassigned.
isUnassigned :: Valuation -> Int -> Bool
isUnassigned val i = getValue val i == NA

-- Set the value of a variable.
setValue :: Int -> Value -> Valuation -> Valuation
setValue = IntMap.insert

-- Initial valuation - all variables unset.
--initValuation :: Int -> Valuation
--initValuation n = IntMap.fromList $ zip [1..n] $ repeat NA
initValuation :: Valuation
initValuation = IntMap.empty

-- Return valuation as list.
getValuation :: Valuation -> [(Int, Value)]
getValuation = filter (\(_, v) -> v /= NA) . IntMap.toList
