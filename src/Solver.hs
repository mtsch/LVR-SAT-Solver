{-# OPTIONS_GHC -Wall #-}
module Solver (evalLit, clauseSatisfied, formulaSatisfied, solve) where

import Types

-- Evaluate a literal.
evalLit :: Valuation -> Literal -> Value
evalLit val lit =
    case lit of
      Lit l -> getValue val l
      Not l -> case getValue val l of
                 TRUE  -> FALSE
                 FALSE -> TRUE
                 NA    -> NA

-- Check whether a clause evaluates to True.
clauseSatisfied :: Valuation -> Clause -> Bool
clauseSatisfied _ [] = False
clauseSatisfied val cls = any (== TRUE) $ map (evalLit val) cls

-- Check whether a formula is satisfied.
formulaSatisfied :: Valuation -> Formula -> Bool
formulaSatisfied _ [] = True
formulaSatisfied val fml = all id $ map (clauseSatisfied val) fml

-- Simplify formula by:
-- + Removing variables that evaluate to FALSE.
-- + Removing clauses that are satisfied.
-- + Unit propagation.
simplifyFormula :: Valuation -> Formula -> (Valuation, Formula)
simplifyFormula valuation = foldr simplify (valuation, [])
    where
      simplify cls (val, cs) =
          case removeFalse val cls of
            lit:[] ->
                case evalLit val lit of
                  TRUE  -> (val, cs)
                  FALSE -> (val, []:cs) -- not reached
                  NA    -> (unitprop lit val, cs)
            c -> if clauseSatisfied val c
                 then (val, cs)
                 else (val, c:cs)
          where
            removeFalse v = filter (\l -> evalLit v l /= FALSE)
            unitprop l =
                setValue (getVariable l) (if isNegated l then FALSE else TRUE)

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

-- Attempt to find a satisfying valuation.
solve :: Formula -> Maybe [(Int, Value)]
solve formula = sat initValuation formula
    where
      sat v f =
          if formulaSatisfied val fml
          then Just $ getValuation val
          else case getFirstNA val fml of
                 Nothing -> Nothing
                 Just i ->
                     case sat (setValue i TRUE val) fml of
                       Just r  -> Just r
                       Nothing -> sat (setValue i FALSE val) fml
              where
                (val, fml) = simplifyFormula v f
