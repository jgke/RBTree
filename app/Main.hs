module Main where

import Criterion.Main
import Data.Bits
import Data.List

import RBTree

xorshift :: Int -> Int
xorshift n = x3
  where x1 = n `xor` (n `shift` (-13))
        x2 = x1 `xor` (x1 `shift` 17)
        x3 = x2 `xor` (x2 `shift` (-5))

randomStream :: [Int]
randomStream = iterate xorshift 1

addNRandom :: RBTree Int -> Int -> RBTree Int
addNRandom tree n = addAll tree $ take (2^n) randomStream

rop :: Int -> RBTree Int -> RBTree Int
rop n tree = case (even n) of
    True -> add n tree
    False -> remove (n+1) tree

opNRandom :: RBTree Int -> Int -> RBTree Int
opNRandom tree n = foldl' (flip rop) tree $ take (2^n) $ map (.&. 65535) randomStream

lookupNRandom :: RBTree Int -> Int -> Bool
lookupNRandom tree n = foldl' (flip $ xor . exists tree) True $ take (2^n) randomStream

testAdd :: Int -> Benchmark
testAdd n = bench ("RBTree add 2^" ++ show n) $ whnf (addNRandom emptyTree) n

testOperate :: Int -> Benchmark
testOperate n = bench ("RBTree operate 2^" ++ show n) $ whnf (opNRandom emptyTree) n

constTree :: RBTree Int
constTree = addNRandom emptyTree 20

testLookup :: Int -> Benchmark
testLookup n = bench ("RBTree lookup 2^" ++ show n) $ whnf (lookupNRandom constTree) n

main :: IO ()
main = defaultMain [ bgroup "add" $ map testAdd [12..16],
                     bgroup "op" $ map testOperate [12..16],
                     bgroup "lookup" $ map testLookup [12..16] ]

