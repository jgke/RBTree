{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib

import Data.List
import Data.Ord
import Data.Foldable
import Control.Monad

tests = testGroup "Tests" [properties, unitTests]

main = defaultMain tests

properties :: TestTree
properties = testGroup "Properties" $ [ scProps, qcProps ]

scProps = testGroup "(checked by SmallCheck)" $ makeTests SC.testProperty
qcProps = testGroup "(checked by QuickCheck)" $ makeTests SC.testProperty

makeTests tester = [ prop tester | prop <- propertyTesters ]

treesAreSorted :: Tester
treesAreSorted a =  a "All trees are sorted" $ \val -> toList (addAll Nil val) == sort val

makeTreeAIsBlack :: Tester
makeTreeAIsBlack a = a "makeTree a is black" $ \val ->
  length val /= 1 || (makeTree (head val) :: RBTree Int) == Node Black (head val) Nil Nil

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
countBlackChildren Nil = 0
countBlackChildren (Node Black _ l r) = 1 + (countBlackChildren l) + (countBlackChildren r)
countBlackChildren (Node Red _ l r) = 0 + (countBlackChildren l) + (countBlackChildren r)

countOfBlackChildrenIsEqual :: Tester
countOfBlackChildrenIsEqual a = a "Count of children is always equal" $ \val -> allTree (\x -> liftM countBlackChildren (getLeft x) == liftM  countBlackChildren (getRight x)) $ addAll Nil val

zipperLawsLeft :: (Ord a) => RBTree a -> Bool
zipperLawsLeft tree = ((Just . zipper) tree >>= takeLeft >>= back >>= (Just . snd)) == Just tree

zipperLawsRight :: (Ord a) => RBTree a -> Bool
zipperLawsRight tree = ((Just . zipper) tree >>= takeLeft >>= back >>= (Just . snd)) == Just tree

type Tester = forall t. ([Char] -> ([Int] -> Bool) -> t) -> t

zipperLeft :: Tester
zipperLeft a = a "takeLeft . back = id" $ \val -> val == [] || zipperLawsLeft (addAll Nil (val :: [Int]))

zipperRight :: Tester
zipperRight a = a "takeRight . back = id" $ \val -> val == [] || zipperLawsRight (addAll Nil (val :: [Int]))


propertyTesters = [treesAreSorted, makeTreeAIsBlack, rootOfTreeIsAlwaysBlack,
                   noAdjacentRedNodes, countOfBlackChildrenIsEqual,
                   zipperLeft, zipperRight]

unitTests = testGroup "Unit tests"
  [ testCase "Simple tree" $
      makeTree "foo" @?= Node Black "foo" Nil Nil
    , testCase "Insert to tree" $
      add "bar" (makeTree "foo") @?= Node Black "foo" (Node Red "bar" Nil Nil) Nil
    , testCase "Insert more to tree" $
      add "bar" (makeTree "foo") @?= Node Black "foo" (Node Red "bar" Nil Nil) Nil

    , testCase "Navigating empty tree" $
      ((Just $ zipper (Nil :: RBTree Int)) >>= takeLeft) @?= Nothing
    , testCase "Navigating one tree" $
      ((Just $ zipper (makeTree 'a')) >>= takeLeft >>= (Just . snd)) @?= Just Nil
    , testCase "Navigating back and forth" $
      ((Just $ zipper (makeTree 'a')) >>= takeLeft >>= back >>= (Just . snd)) @?= Just (makeTree 'a')
    --, testCase "Insert many more to tree" $
    -- addAll (makeTree 4) [1, 2, 3, 4, 5] @?= Black 4 (Black 3 (Black 2 (Black 1 Nil Nil) Nil) Nil) (Black 5 (Black 4 Nil Nil) Nil)
  ]
