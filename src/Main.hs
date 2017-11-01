{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import System.Environment
import System.IO

import Data.Set (Set)
import qualified Data.Set as Set

import Solver
import Parser

resultToString :: Set (Int, Bool) -> String
resultToString = concatMap showVar . Set.toList
    where
      showVar (i, v) =
          if v
          then show i ++ " "
          else "-" ++ show i ++ " "

-- Read file, solve the problem and write file.
crunchFile :: String -> String -> IO ()
crunchFile infile outfile =
    readFile infile >>= return . readDIMACS >>= \formula ->
        case formula of
          Nothing -> hPutStrLn stderr "Error: Invalid file!"
          Just f  -> case solve f of
                       Nothing  -> writeFile outfile "0"
                       Just val -> writeFile outfile $ resultToString val

main :: IO ()
main = getArgs >>= \args ->
       case args of
         input:output:_ -> crunchFile input output
         _ -> hPutStrLn stderr "Error: Not enough command line arguments!"
