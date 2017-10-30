module Evaluate (evalLit, clauseSatisfied, formulaSatisfied, satisfy) where

import Types

evalLit :: Valuation -> Literal -> Value
evalLit val lit =
    case lit of
      Lit l -> getValue val l
      Not l -> case getValue val l of
                 TRUE -> FALSE
                 FALSE -> TRUE
                 NA -> NA

clauseSatisfied :: Valuation -> Clause -> Bool
clauseSatisfied _ [] = True
clauseSatisfied val cls = any (== TRUE) $ map (evalLit val) cls

formulaSatisfied :: Valuation -> Formula -> Bool
formulaSatisfied _ [] = True
formulaSatisfied val fml = all id $ map (clauseSatisfied val) fml

-- Find the first unassigned variable in formula.
getFirstNA :: Valuation -> Formula -> Maybe Int
getFirstNA val formula =
    case formula of
      []   -> Nothing
      [[]] -> Nothing
      _    -> case concat $ (map . filter) (isUnassigned val) $ f of
                []    -> Nothing
                (l:_) -> Just l
          where
            f = (map . map) getVariable $ formula

satisfy :: Int -> Formula -> SATResult
satisfy nvars formula = sat (initValuation nvars) formula
    where sat val fml =
              if formulaSatisfied val fml
              then Satisfied $ getValuation val
              else case getFirstNA val fml of
                     Nothing -> Unsatisfiable
                     Just i ->
                         case sat (setValue i TRUE val) fml of
                           Satisfied v   -> Satisfied v
                           Unsatisfiable -> sat (setValue i FALSE val) fml
