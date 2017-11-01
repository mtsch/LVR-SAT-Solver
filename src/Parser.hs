module Parser (readDIMACS) where

import Data.Char
import Data.Maybe
import qualified Data.Set as Set

import Solver

-- Filter out lines starting with 'c' or empty lines.
removeCommentsOrEmpty :: [String] -> [String]
removeCommentsOrEmpty = filter (\l -> (not . null) l && head l /= 'c')

-- Read a single literal.
readLiteral :: String -> Maybe Literal
readLiteral str
    | null str                   = Nothing
    | c == '-' && all isDigit cs = Just . Not $ read cs
    | all isDigit str            = Just . Lit $ read str
    | otherwise                  = Nothing
    where
      c  = head str
      cs = tail str

-- Read a clause.
readClause :: String -> Maybe Clause
readClause str =
    if all isJust lits
    then Just . Set.fromList $ map fromJust lits
    else Nothing
    where
      tkns = tail . reverse $ words str -- remove trailing 0
      lits = map readLiteral tkns

-- Extract the number of variables from header.
readNVars :: String -> Maybe Int
readNVars str =
    case words str of
      "p":"cnf":n:_:[] -> if all isDigit n
                          then Just $ read n
                          else Nothing
      _ -> Nothing

-- Read a string in DIMACS format.
readDIMACS :: String -> Maybe Formula
readDIMACS text =
    case removeCommentsOrEmpty $ lines text of
      l:ls -> let n   = readNVars l
                  cls = map readClause ls
              in if isJust n && all isJust cls
                 then Just $ map fromJust cls
                 else Nothing
      _ -> Nothing
