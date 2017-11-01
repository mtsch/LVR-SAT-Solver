{-# OPTIONS_GHC -Wall #-}
module Solver (Literal (..), Clause, Formula, solve) where

import Data.List

data Literal = Lit Int
             | Not Int

instance Show Literal where
    show (Lit l) = "+" ++ show l
    show (Not l) = "-" ++ show l

type Clause = [Literal]

type Formula = [Clause]

-- Get the label of variable.
getLabel :: Literal -> Int
getLabel (Lit i) = i
getLabel (Not i) = i

-- Convert the Literal to a (label, negated?) pair.
toPair :: Literal -> (Int, Bool)
toPair (Lit i) = (i, True)
toPair (Not i) = (i, False)

-- Set multiple variables in a formula.
assign :: [(Int, Bool)] -> Formula -> Formula
assign vals = foldr set []
    where
      set c cs =
          case intersect vals pairs of
            (_:_) -> cs   -- remove on true assignments
            [] -> (c':cs) -- filter out false assignments
          where
            pairs = map toPair c
            vars  = map fst vals
            c'    = filter (\l -> not $ getLabel l `elem` vars) c

-- Unit propagation - handle all unit clauses at once.
unitPropagate :: [(Int, Bool)] -> Formula -> ([(Int, Bool)], Formula)
unitPropagate val fml = (assignments ++ val, assign assignments fml)
    where
      units = map head $ filter (null . tail) fml
      assignments = map toPair units

-- Solve the SAT problem.
solve :: Formula -> Maybe [(Int, Bool)]
solve formula = sat [] formula
    where
      sat val [] = Just val
      sat val fml =
          if any null fml
          then Nothing
          else case fml' of
                 [] -> Just val
                 _  -> if any null fml'
                       then Nothing
                       else case sat valT fmlT of
                              Just r  -> Just r
                              Nothing -> sat valF fmlF
          where
            (val', fml') = unitPropagate val fml
            i = getLabel . head . head $ fml'
            fmlT = assign [(i, True)] fml'
            valT = (i, True):val'
            fmlF = assign [(i, False)] fml'
            valF = (i, False):val'
