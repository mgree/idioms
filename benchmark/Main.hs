-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Idioms.Grams

main :: IO ()
main = defaultMain 
       [bgroup "Books"
        [ bench "train all" $ nfIO (train ["txts/ulysses.txt", "txts/mobydick.txt"])
        , bench "ulysses"   $ nfIO (train ["txts/ulysses.txt"])
        , bench "moby dick" $ nfIO (train ["txts/mobydick.txt"])
        ]
       ]
