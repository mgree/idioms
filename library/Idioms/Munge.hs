module Idioms.Munge where

import Data.Char

text :: String -> [String]
text = map (map toLower) . words

textFile :: FilePath -> IO [String]
textFile f = text <$> readFile f
