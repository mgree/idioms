{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Main where

import System.Environment
import System.Exit

import qualified Data.Map as Map

import Idioms.Grams

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    fs -> do
      m <- train fs
      let twos = Map.size $ twoGrams m
      let threes = Map.size $ threeGrams m
      putStrLn $ "Trained. Found " ++ show twos ++ " 2-grams and " ++ show threes ++ " 3-grams."
      
usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ "Usage: " ++ prog ++ " file1 file2 ..."
  exitFailure