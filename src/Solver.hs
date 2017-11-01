{-# OPTIONS_GHC -Wall #-}
module Solver (Literal (..), Clause, Formula, solve) where

import Data.Set (Set)
import qualified Data.Set as Set

data Literal = Lit Int
             | Not Int
               deriving (Eq)

instance Show Literal where
    show (Lit l) = "+" ++ show l
    show (Not l) = "-" ++ show l

instance Ord Literal where
    compare (Lit i) (Not j) = if i == j then GT else compare i j
    compare l1 l2 = compare (getLabel l1) (getLabel l2)

type Clause = Set Literal

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
assign :: Set (Int, Bool) -> Formula -> Formula
assign vals = foldr assign' []
    where
      assign' c cs =
          case Set.intersection vals pairs of
            int
              | Set.null int -> (c':cs)
              | otherwise    -> cs -- remove on true assignments
          where
            pairs = Set.map toPair c
            vars  = Set.map fst vals
            c'    = Set.filter (\l -> Set.notMember (getLabel l) vars) c

-- Unit propagation - handle all unit clauses at once.
unitPropagate :: Set (Int, Bool) -> Formula -> (Set (Int, Bool), Formula)
unitPropagate val fml = (Set.union assignments val, assign assignments fml)
    where
      units = map (Set.elemAt 0) $ filter isUnit fml
      assignments = Set.fromList $ map toPair units
      isUnit c = Set.size c == 1

-- Solve the SAT problem.
solve :: Formula -> Maybe (Set (Int, Bool))
solve formula = sat Set.empty formula
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
            i = getLabel . Set.elemAt 0 . head $ fml'
            fmlT = assign (Set.singleton (i, True)) fml'
            valT = Set.insert (i, True) val'
            fmlF = assign (Set.singleton (i, False)) fml'
            valF = Set.insert (i, False) val'
