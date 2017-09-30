-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Idioms.Grams

main :: IO ()
main = defaultMain 
       [bgroup "Books"
        [ bench "train all (trainText)" $ nfIO (trainText ["txts/ulysses.txt", "txts/mobydick.txt"])
        , bench "ulysses (trainText)"   $ nfIO (trainText ["txts/ulysses.txt"])
        , bench "moby dick (trainText)" $ nfIO (trainText ["txts/mobydick.txt"])

        , bench "train all (trainPar)" $ nfIO (trainPar ["txts/ulysses.txt", "txts/mobydick.txt"])
        , bench "ulysses (trainPar)"   $ nfIO (trainPar ["txts/ulysses.txt"])
        , bench "moby dick (trainPar)" $ nfIO (trainPar ["txts/mobydick.txt"])
        ]
       ]
