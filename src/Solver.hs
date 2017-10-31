{-# OPTIONS_GHC -Wall #-}
module Solver (evalLit, clauseSatisfied, formulaSatisfied, solve) where

import Types

-- Evaluate a literal.
evalLit :: Valuation -> Literal -> Value
evalLit val lit =
    case lit of
      Lit l -> getValue val l
      Not l -> case getValue val l of
                 TRUE -> FALSE
                 FALSE -> TRUE
                 NA -> NA

-- Check whether a clause evaluates to True.
clauseSatisfied :: Valuation -> Clause -> Bool
clauseSatisfied _ [] = True
clauseSatisfied val cls = any (== TRUE) $ map (evalLit val) cls

-- Check whether a formula is satisfied.
formulaSatisfied :: Valuation -> Formula -> Bool
formulaSatisfied _ [] = True
formulaSatisfied val fml = all id $ map (clauseSatisfied val) fml

-- Find the first unassigned variable in formula.
getFirstNA :: Valuation -> Formula -> Maybe Int
getFirstNA val formula =
    case formula of
      []   -> Nothing
      [[]] -> Nothing
      _    -> case (concatMap . filter) (isUnassigned val) $ f of
                []    -> Nothing
                (l:_) -> Just l
          where
            f = (map . map) getVariable $ formula

-- Remvoe satisfied clauses from formula.
removeSatisfied :: Valuation -> Formula -> Formula
removeSatisfied val = filter (not . clauseSatisfied val)

-- Attempt to find a satisfying valuation.
solve :: Formula -> Maybe [(Int, Value)]
solve formula = sat initValuation formula
    where
      sat val formula =
          if formulaSatisfied val fml
          then Just $ getValuation val
          else case getFirstNA val fml of
                 Nothing -> Nothing
                 Just i ->
                     case sat (setValue i TRUE val) fml of
                       Just v  -> Just v
                       Nothing -> sat (setValue i FALSE val) fml
              where
                fml = removeSatisfied val formula
