module Main where

import Criterion.Main
import Data.Bits
import Data.List
import Control.DeepSeq

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

addNRandomUnbalanced :: UnbalancedTree Int -> Int -> UnbalancedTree Int
addNRandomUnbalanced tree n = foldl' (flip addUnbalanced) tree $ take (2^n) randomStream

rop :: Int -> RBTree Int -> RBTree Int
rop n tree = case (even n) of
    True -> add n tree
    False -> remove (n+1) tree

opNRandom :: RBTree Int -> Int -> RBTree Int
opNRandom tree n = foldl' (flip rop) tree $ take (2^n) $ map (.&. 65535) randomStream

lookupNRandom :: RBTree Int -> Int -> Bool
lookupNRandom tree n = foldl' (flip $ xor . exists tree) True $ take (2^n) randomStream

testAddRandom :: Int -> Benchmark
testAddRandom n = bench ("RBTree add random 2^" ++ show n) $ nf (addNRandom emptyTree) n

testAddOrdered :: Int -> Benchmark
testAddOrdered n = bench ("RBTree add ordered 2^" ++ show n) $ nf (addAll emptyTree) [1..(2^n)]

testAddUnbalanced :: Int -> Benchmark
testAddUnbalanced n = bench ("RBTree add to unbalanced 2^" ++ show n) $ nf (addNRandomUnbalanced UBNil) n

testAddUnbalancedOrdered :: Int -> Benchmark
testAddUnbalancedOrdered n = bench ("RBTree add to unbalanced ordered 2^" ++ show n) $ nf (addAllUnbalanced UBNil) [1..(2^n)]

testOperate :: Int -> Benchmark
testOperate n = bench ("RBTree operate 2^" ++ show n) $ nf (opNRandom emptyTree) n

constTree :: RBTree Int
constTree = addNRandom emptyTree 20

testLookup :: Int -> Benchmark
testLookup n = bench ("RBTree lookup 2^" ++ show n) $ nf (lookupNRandom constTree) n

bigRange :: [Int]
bigRange = [12..18]

smallRange :: [Int]
smallRange = [12..13]

main :: IO ()
main = defaultMain [ bgroup "add UnbalancedTree" $ map testAddUnbalanced bigRange,
                     bgroup "add UnbalancedTree ordered" $ map testAddUnbalancedOrdered smallRange,
                     bgroup "addRandom" $ map testAddRandom bigRange,
                     bgroup "addOrdered" $ map testAddOrdered bigRange,
                     bgroup "op" $ map testOperate bigRange,
                     bgroup "lookup" $ map testLookup bigRange ]

data UnbalancedTree a = UBNil
                      | UBNode a !(UnbalancedTree a) !(UnbalancedTree a)

addUnbalanced :: (Ord a) =>  a -> UnbalancedTree a -> UnbalancedTree a
addUnbalanced value UBNil = UBNode value UBNil UBNil
addUnbalanced value n@(UBNode treeValue l r) = case (compare value treeValue) of
    LT -> (UBNode treeValue (addUnbalanced value l) r)
    EQ -> n
    GT -> (UBNode treeValue l (addUnbalanced value r))

addAllUnbalanced :: (Ord a, Foldable t) => UnbalancedTree a -> t a -> UnbalancedTree a
addAllUnbalanced tree coll = foldr addUnbalanced tree coll

instance NFData (UnbalancedTree a) where
    rnf a = seq a ()

instance NFData (RBTree a) where
    rnf a = seq a ()

lengthUB :: UnbalancedTree a -> Int
lengthUB UBNil = 0
lengthUB (UBNode _ l r) = 1 + (lengthUB l) + (lengthUB r)
