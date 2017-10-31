{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import System.Environment
import System.IO

import Solver
import Parser
import Types

resultToString :: [(Int, Value)] -> String
resultToString = concatMap showVar
    where
      showVar (i, v) = case v of
                         TRUE  -> show i ++ " "
                         FALSE -> "-" ++ show i ++ " "
                         NA    -> ""

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
