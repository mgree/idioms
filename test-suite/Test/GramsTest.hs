module Test.GramsTest where

import qualified Data.Map as Map
import Data.Map (Map)

import Test.QuickCheck

import Idioms.Grams

a2z :: Model Char
a2z = emptyModel `trainOn` ['a'..'z']

every :: Ord k => (k -> a -> Bool) -> Map k a -> Bool
every p = Map.foldWithKey (\k v ok -> ok && p k v) True

prop_a2z_twoGrams_allOne :: Bool
prop_a2z_twoGrams_allOne = every (\(a,b) count -> count == 1 && b == succ a) (twoGrams a2z)

prop_a2z_threeGrams_allOne :: Bool
prop_a2z_threeGrams_allOne = 
  every (\(a,b,c) count -> count == 1 && c == succ b && b == succ a) (threeGrams a2z)

prop_train_monotonic :: [String] -> [String] -> Property
prop_train_monotonic as bs = length as >= 3 && length bs >= 3 ==>
  let ma = emptyModel `trainOn` as
      mb = emptyModel `trainOn` bs
      mab = emptyModel `trainOn` (as ++ bs) in
  every (\two count -> Map.findWithDefault 0 two (twoGrams ma) <= count) (twoGrams mab) &&
  every (\two count -> Map.findWithDefault 0 two (twoGrams mb) <= count) (twoGrams mab) &&
  every (\three count -> Map.findWithDefault 0 three (threeGrams ma) <= count) (threeGrams mab) &&
  every (\three count -> Map.findWithDefault 0 three (threeGrams mb) <= count) (threeGrams mab)
  