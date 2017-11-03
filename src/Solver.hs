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
    compare l1 l2 = compare (toPair l1) (toPair l2)

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

-- Convert the Literal to a negated (label, negated?) pair.
-- toNegPair :: Literal -> (Int, Bool)
-- toNegPair (Lit i) = (i, False)
-- toNegPair (Neg i) = (i, True)

-- Convert pair to Literal.
fromPair :: (Int, Bool) -> Literal
fromPair (i, True)  = Lit i
fromPair (i, False) = Neg i

negateLit :: Literal -> Literal
negateLit (Lit i) = (Neg i)
negateLit (Neg i) = (Lit i)

-- Set multiple variables in a formula by removing clauses and literals.
assign :: Set (Int, Bool) -> Formula -> Maybe Formula
assign vals = foldr assign' (Just [])
    where
      vars = Set.mapMonotonic fromPair vals
      assign' _ Nothing   = Nothing
      assign' c (Just cs) =
          case Set.intersection vars c of
            int
              | not $ Set.null int -> Just cs -- Clause contains the assignment.
              | Set.null c'        -> Nothing -- Unsatisfiable clause.
              | otherwise          -> Just (c':cs)
          where
            c' = Set.filter (\l -> Set.notMember (negateLit l) vars) c

-- Unit propagation - handle all unit clauses at once.
unitPropagate :: Valuation -> Formula -> Maybe (Valuation, Formula)
unitPropagate val fml =
    case fml' of
      Nothing -> Nothing
      Just f  -> Just (val', f)
    where
      units  = Set.fromList . map (toPair . Set.elemAt 0) . filter isUnit $ fml
      isUnit = (1 ==) . Set.size
      val'   = Set.union units val
      fml'   = assign units fml

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
            Nothing           -> Nothing
            Just (val', [])   -> Just val'
            Just (val', fml') ->
                case sat valT fmlT of
                  Just r  -> Just r
                  Nothing -> sat valF fmlF
                where
                  lab  = getLabel . Set.elemAt 0 . head $ fml'
                  fmlT = assign (Set.singleton (lab, True)) fml'
                  valT = Set.insert (lab, True) val'
                  fmlF = assign (Set.singleton (lab, False)) fml'
                  valF = Set.insert (lab, False) val'
