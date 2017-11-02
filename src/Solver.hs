{-# OPTIONS_GHC -Wall #-}
module Solver (Literal (..), Clause, Formula, Valuation, solve) where

import Data.Set (Set)
import qualified Data.Set as Set

data Literal = Lit Int
             | Neg Int
               deriving (Eq)

instance Show Literal where
    show (Lit l) = show l
    show (Neg l) = "-" ++ show l

instance Ord Literal where
    compare (Lit i) (Neg j) = if i == j then GT else compare i j
    compare (Neg i) (Lit j) = if i == j then LT else compare i j
    compare l1 l2           = compare (getLabel l1) (getLabel l2)

type Clause = Set Literal

type Formula = [Clause]

type Valuation = Set (Int, Bool)

-- Get the label of variable.
getLabel :: Literal -> Int
getLabel (Lit i) = i
getLabel (Neg i) = i

-- Convert the Literal to a (label, negated?) pair.
toPair :: Literal -> (Int, Bool)
toPair (Lit i) = (i, True)
toPair (Neg i) = (i, False)

-- Set multiple variables in a formula by removing clauses and literals.
assign :: Valuation -> Formula -> Maybe Formula
assign vals = foldr assign' (Just [])
    where
      assign' _ Nothing   = Nothing
      assign' c (Just cs) =
          case Set.intersection vals pairs of
            int
              | not $ Set.null int -> Just cs
              | Set.null c'        -> Nothing
              | otherwise          -> Just (c':cs)
          where
            pairs = Set.mapMonotonic toPair c
            vars  = Set.mapMonotonic fst vals
            c'    = Set.filter (\l -> Set.notMember (getLabel l) vars) c

-- Unit propagation - handle all unit clauses at once.
unitPropagate :: Valuation -> Formula -> Maybe (Valuation, Formula)
unitPropagate val fml =
    case fml' of
      Nothing -> Nothing
      Just f  -> Just (val', f)
    where
      units       = Set.fromList . map (Set.elemAt 0) . filter isUnit $ fml
      assignments = Set.mapMonotonic toPair units
      isUnit c    = Set.size c == 1
      val'        = Set.union assignments val
      fml'        = assign assignments fml

-- Solve the SAT problem.
solve :: Formula -> Maybe Valuation
solve formula = if any null formula
                then Nothing
                else sat Set.empty $ Just formula
    where
      sat _ Nothing      = Nothing
      sat val (Just [])  = Just val
      sat val (Just fml) =
          case unitPropagate val fml of
            Nothing         -> Nothing
            Just (val', []) -> Just val'
            Just (val', fml') ->
                case sat valT fmlT of
                  Just r  -> Just r
                  Nothing -> sat valF fmlF
                where
                  i     = getLabel . Set.elemAt 0 . head $ fml'
                  fmlT  = assign (Set.singleton (i, True)) fml'
                  valT  = Set.insert (i, True) val'
                  fmlF  = assign (Set.singleton (i, False)) fml'
                  valF  = Set.insert (i, False) val'
