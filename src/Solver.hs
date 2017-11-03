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
    compare (Lit i) (Lit j) = if i == j then EQ else compare i j
    compare (Neg i) (Neg j) = if i == j then EQ else compare i j

type Clause = Set Literal

type Formula = [Clause]

type Valuation = Set Literal

-- Negate literal.
negateLit :: Literal -> Literal
negateLit (Lit i) = (Neg i)
negateLit (Neg i) = (Lit i)

-- Set multiple variables in a formula by removing clauses and literals.
assign :: Valuation -> Formula -> Maybe Formula
assign vars = foldr assign' (Just [])
    where
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
      units  = Set.fromList . map (Set.elemAt 0) . filter isUnit $ fml
      isUnit = (1 ==) . Set.size
      fml'   = assign units fml
      val'   = Set.union units val

-- Convert the Literal to a (label, negated?) pair.
toPair :: Literal -> (Int, Bool)
toPair (Lit i) = (i, True)
toPair (Neg i) = (i, False)

-- Solve the SAT problem.
solve :: Formula -> Maybe (Set (Int, Bool))
solve formula = if any null formula
                then Nothing
                else sat Set.empty (Just formula) >>=
                     return . Set.mapMonotonic toPair
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
                  lit  = Set.elemAt 0 . head $ fml'
                  nlit = negateLit lit
                  fmlT = assign (Set.singleton lit) fml'
                  valT = Set.insert lit val'
                  fmlF = assign (Set.singleton nlit) fml'
                  valF = Set.insert nlit val'
