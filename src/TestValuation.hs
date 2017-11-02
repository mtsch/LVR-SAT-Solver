{- This module is intended to be run with runhaskell and is used to test whether
 - a valuation satisfies a formula. -}
module TestValuation where

import Data.Char
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import System.Environment
import System.IO

import Parser
import Solver

-- Read a clause.
readValuation :: String -> Maybe Valuation
readValuation str =
    if all isJust lits
    then Just . Set.fromList $ map fromJust lits
    else Nothing
    where
      tkns = words str
      lits = map readVar tkns

-- Read a value literal.
readVar :: String -> Maybe (Int, Bool)
readVar str
    | null str                   = Nothing
    | c == '-' && all isDigit cs = Just (read cs, False)
    | all isDigit str            = Just (read str, True)
    | otherwise                  = Nothing
    where
      c  = head str
      cs = tail str

-- Eval a formula.
evalLit :: Valuation -> Literal -> Bool
evalLit val (Lit l) = Set.member (l, True) val
evalLit val (Neg l) = Set.member (l, False) val

evalClause :: Valuation -> Clause -> Bool
evalClause val = Set.member True . Set.map (evalLit val)

evalFormula :: Valuation -> Formula -> Bool
evalFormula val = all (evalClause val)

-- Check a valuation and print the result to stdout.
checkValuation :: String -> String -> IO ()
checkValuation valFile fmlFile =
    do val <- readFile valFile >>= return . readValuation
       fml <- readFile fmlFile >>= return . readDIMACS
       case (val, fml) of
         (Nothing, Nothing) -> hPutStrLn stderr "Error: Both files invalid!"
         (Nothing, _)       -> hPutStrLn stderr "Error: Invalid valuation file!"
         (_, Nothing)       -> hPutStrLn stderr "Error: Invalid formula file!"
         (Just v, Just f)   -> if evalFormula v f
                               then putStrLn "Formula is satisfied."
                               else putStrLn "Formula NOT SATISFIED."

main :: IO ()
main = getArgs >>= \args ->
       case args of
         fml:val:_ -> checkValuation val fml
         _ -> hPutStrLn stderr "Error: Neg enough command line aruments!"
