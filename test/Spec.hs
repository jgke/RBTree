{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib

import Data.List
import Data.Foldable
import Control.Monad
import Data.Maybe (isJust)

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

main :: IO ()
main = defaultMain tests

properties :: TestTree
properties = testGroup "Properties" $ [ scProps, qcProps ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" $ makeTests SC.testProperty

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" $ makeTests QC.testProperty

makeTests :: ([Char] -> ([Int] -> Bool) -> a) -> [a]
makeTests tester = [ prop tester | prop <- propertyTesters ]

treesAreSorted :: Tester
treesAreSorted a =  a "All trees are sorted" $ \val -> toList (addAll Nil val) == sort val

makeTreeAIsBlack :: Tester
makeTreeAIsBlack a = a "makeTree a is black" $ \val ->
  (length val /= 1) || (makeTree (head val) :: RBTree Int) == Node Black (head val) Nil Nil

rootOfTreeIsAlwaysBlack :: Tester
rootOfTreeIsAlwaysBlack a = a "Root of tree is always black" $ \val -> isBlack $ makeTree val

childIsRed :: RBTree a -> Bool
childIsRed Nil = False
childIsRed (Node _ _ l r)  = isRed l || isRed r

childAndParentIsRed :: RBTree a -> Bool
childAndParentIsRed tree = isRed tree && childIsRed tree

noAdjacentRedNodes :: Tester
noAdjacentRedNodes a = a "There are no adjacent red nodes" $ \val -> allTree (not . childAndParentIsRed) $ addAll Nil val

countBlackChildren :: RBTree a -> Int
countBlackChildren Nil = 1
countBlackChildren (Node _ _ l r) = max (countBlackChildren l) (countBlackChildren r)

countOfBlackChildrenIsEqual :: Tester
countOfBlackChildrenIsEqual a = a "Count of black children is always equal" $ \val -> allTree (\x -> liftM countBlackChildren (getLeft x) == liftM  countBlackChildren (getRight x)) $ addAll Nil val

zipperLawsLeft :: (Ord a) => RBTree a -> Bool
zipperLawsLeft tree = ((Just . zipper) tree >>= takeLeft >>= back >>= (Just . snd)) == Just tree

zipperLawsRight :: (Ord a) => RBTree a -> Bool
zipperLawsRight tree = ((Just . zipper) tree >>= takeLeft >>= back >>= (Just . snd)) == Just tree

type Tester = forall t. ([Char] -> ([Int] -> Bool) -> t) -> t

zipperLeft :: Tester
zipperLeft a = a "takeLeft . back = id" $ \val -> val == [] || zipperLawsLeft (addAll Nil (val :: [Int]))

zipperRight :: Tester
zipperRight a = a "takeRight . back = id" $ \val -> val == [] || zipperLawsRight (addAll Nil (val :: [Int]))

searchSome :: [Int] -> Bool
searchSome (x:xs) = (not $ elem x xs) || (isJust $ search (addAll Nil xs) x)
searchSome _ = error ""

searchWorks :: Tester
searchWorks a = a "Searching tree" $ \val -> val == [] || searchSome val

propertyTesters :: [([Char] -> ([Int] -> Bool) -> t) -> t]
propertyTesters = [treesAreSorted, makeTreeAIsBlack, rootOfTreeIsAlwaysBlack,
                   noAdjacentRedNodes, countOfBlackChildrenIsEqual,
                   zipperLeft, zipperRight, searchWorks]

makeIntTree :: Int -> RBTree Int
makeIntTree = makeTree

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Simple tree" $
      makeIntTree 1 @?= Node Black 1 Nil Nil
    , testCase "Insert to tree" $
      add 2 (makeIntTree 1) @?= Node Black 1 Nil (Node Red 2 Nil Nil)
    , testCase "Navigating empty tree" $
      ((Just $ zipper (Nil :: RBTree Int)) >>= takeLeft) @?= Nothing
    , testCase "Navigating one tree" $
      ((Just $ zipper (makeIntTree 0)) >>= takeLeft >>= (Just . snd)) @?= Just Nil
    , testCase "Navigating back and forth" $
      ((Just $ zipper (makeIntTree 0)) >>= takeLeft >>= back >>= (Just . snd)) @?= Just (makeTree 0)
    , testCase "Insert many more to tree" $
      addAll (makeIntTree 4) [1, 2, 3, 4, 5] @?= Node Black 4 (Node Red 3 (Node Black 2 (Node Red 1 Nil Nil) Nil) (Node Black 4 Nil Nil)) (Node Black 5 Nil Nil)
    , testCase "Search from Nil" $
      search Nil "foo" @?= Nothing
    , testCase "Search from simple tree" $
      search (makeIntTree 1) 1 @?= Just 1
    , testCase "Search from simple tree" $
      search (makeIntTree 0) 1 @?= Nothing
    , testCase "Search from larger tree" $
      search (addAll (makeIntTree 4) [1, 2, 3, 4, 5]) 1 @?= Just 1
    , testCase "Search from larger tree" $
      search (addAll (makeIntTree 4) [1, 2, 3, 4, 5]) 0 @?= Nothing
  ]
