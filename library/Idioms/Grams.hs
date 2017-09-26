{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Idioms.Grams where

import Control.DeepSeq

import Data.Map (Map)
import qualified Data.Map as Map

data Model a = 
  Model { twoGrams :: Map (a,a) Int,
          threeGrams :: Map (a,a,a) Int }
  deriving (Eq, Ord, Show)
           
instance NFData a => NFData (Model a) where
  rnf m = seq m $ seq (rnf $ twoGrams m) $ seq (rnf $ threeGrams m) ()
                       
emptyModel :: Model a
emptyModel = Model Map.empty Map.empty

joinModels :: Ord a => [Model a] -> Model a
joinModels ms = Model { twoGrams = Map.unions (map twoGrams ms),
                        threeGrams = Map.unions (map threeGrams ms) }

countTwo :: Ord a => (a,a) -> Model a -> Model a
countTwo two m = m { twoGrams = Map.insertWith (+) two 1 (twoGrams m) }

countThree :: Ord a => (a,a,a) -> Model a -> Model a
countThree three m = m { threeGrams = Map.insertWith (+) three 1 (threeGrams m) }

grams :: Ord a => [a] -> Model a -> Model a
grams []             m = m
grams [_]            m = m -- only on symbol, leave it
grams [a,b]          m = countTwo (a,b) m
grams [a,b,c]        m = countThree (a,b,c) $ countTwo (b,c) $ m
grams (a:b:c:d:rest) m = grams (c:d:rest) $ countThree (a,b,c) $ countTwo (b,c) $ m
  
trainOn :: Ord a => Model a -> [a] -> Model a
trainOn m l@(a:b:_) = grams l $ countTwo (a,b) m
trainOn m _ = m -- only one symbol, leave it

-- TODO what are the smoothing tricks for novel symbols?
-- TODO move to log?
followedBy :: Ord a => Model a -> (a,a) -> a -> Float
followedBy m (a,b) c = (fromIntegral abc) / (fromIntegral ab)
  where abc = Map.findWithDefault 0 (a,b,c) (threeGrams m)
        ab = Map.findWithDefault 0 (a,b) (twoGrams m)
        
precededBy :: Ord a => Model a -> a -> (a,a) -> Float
precededBy m a (b,c) = (fromIntegral abc) / (fromIntegral bc)
  where abc = Map.findWithDefault 0 (a,b,c) (threeGrams m)
        bc = Map.findWithDefault 0 (b,c) (twoGrams m)
        
next :: Ord a => Model a -> (a,a) -> Map a Float
next m (a,b) = Map.map (\count -> fromIntegral count / fromIntegral ab) cs
  where ab = Map.findWithDefault 0 (a,b) (twoGrams m)
        abcs = Map.filterWithKey (\(a',b',_) _ -> a == a' && b == b') (threeGrams m)
        cs = Map.mapKeysMonotonic (\(_,_,c') -> c') abcs
        
train :: [FilePath] -> IO (Model String)
train fs = joinModels <$> mapM trainOnFile fs
  where trainOnFile f = do
          text <- words <$> readFile f
          return $ trainOn emptyModel text

books :: IO (Model String)
books = train ["../literary_ebooks/mobydick.txt",
               "../literary_ebooks/ulysses.txt"]