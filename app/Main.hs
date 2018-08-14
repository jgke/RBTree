module Main where

import Criterion.Main
import Data.Bits
import Data.List
import Lib

xorshift :: Int -> Int
xorshift n = x3
  where x1 = n `xor` (n `shift` (-13))
        x2 = x1 `xor` (x1 `shift` 17)
        x3 = x2 `xor` (x2 `shift` (-5))

addNRandom :: Int -> RBTree Int
addNRandom n = foldl' (flip add) Nil $ take (2^n) $ iterate xorshift 1

main :: IO ()
main = defaultMain [ bgroup "add" [
            bench "RBTree add 2^8" $ whnf addNRandom 8,
            bench "RBTree add 2^9" $ whnf addNRandom 9,
            bench "RBTree add 2^10" $ whnf addNRandom 10,
            bench "RBTree add 2^11" $ whnf addNRandom 11,
            bench "RBTree add 2^l2" $ whnf addNRandom 12,
            bench "RBTree add 2^13" $ whnf addNRandom 13,
            bench "RBTree add 2^14" $ whnf addNRandom 14
        ]
    ]
