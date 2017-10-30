{-# OPTIONS_GHC -Wall #-}
module Types ( Literal (..)
             , Clause
             , Formula
             , Value (..)
             , Valuation
             , SATResult (..)
             , getVariable
             , getValue
             , getLitValue
             , isUnassigned
             , setValue
             , initValuation
             , getValuation
             , isSatisfied
             ) where

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

data Literal = Lit Int
             | Not Int

instance Show Literal where
    show (Lit l) = "+" ++ show l
    show (Not l) = "-" ++ show l

instance Read Literal where
    readsPrec _ (c:cs) = if c == '-'
                         then [(Not $ read cs, "")]
                         else [(Lit $ read (c:cs), "")]
    readsPrec _ _ = undefined

type Clause = [Literal]

type Formula = [Clause]

-- True, False or Unassigned.
data Value = TRUE | FALSE | NA
             deriving (Show, Eq)

-- Collection of variables and their values.
type Valuation = IntMap Value

-- The result of the SAT problem.
data SATResult = Unsatisfiable
               | Satisfied [(Int, Value)]
                 deriving (Show, Eq)

-- Get the variable name from literal
getVariable :: Literal -> Int
getVariable (Lit i) = i
getVariable (Not i) = i

-- Get value of variable from valuation.
getValue :: Valuation -> Int -> Value
getValue = (IntMap.!)

getLitValue :: Valuation -> Literal -> Value
getLitValue val = getValue val . getVariable

-- Check if variable is unassigned.
isUnassigned :: Valuation -> Int -> Bool
isUnassigned val i = getValue val i == NA

-- Set the value of a variable.
setValue :: Int -> Value -> Valuation -> Valuation
setValue = IntMap.insert

-- Initial valuation - all variables unset.
initValuation :: Int -> Valuation
initValuation n = IntMap.fromList $ zip [1..n] $ repeat NA

-- Return valuation as list.
getValuation :: Valuation -> [(Int, Value)]
getValuation = filter (\(_, v) -> v /= NA) . IntMap.toList

-- Check if formula is satisfied.
isSatisfied :: SATResult -> Bool
isSatisfied Unsatisfiable = False
isSatisfied _ = True
