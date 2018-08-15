{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCSeries
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib

import Data.List
import Data.Foldable
import Data.Maybe (isJust)
import Data.Functor

data Operation a = Insert a | Delete a deriving (Show)
instance Arbitrary (Operation Int) where
  arbitrary = do
    val <- choose (1, 10)
    oneof [return $ Insert val, return $ Delete val]

instance Serial m a => Serial m (Operation a) where
  series = cons1 Insert \/ cons1 Delete

operate :: (Ord a) => RBTree a -> [Operation a] -> RBTree a
operate tree [] = tree
operate tree (Insert item:xs) = operate (add item tree) xs
operate tree (Delete item:xs) = operate (Lib.delete item tree) xs

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

main :: IO ()
main = defaultMain tests

properties :: TestTree
properties = testGroup "Properties" $ [ scProps, qcProps ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)" $ makeTests SC.testProperty ++ [treeIsValidAfterAnyOperation SC.testProperty]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)" $ makeTests QC.testProperty ++ [treeIsValidAfterAnyOperation QC.testProperty]

makeTests :: ([Char] -> ([Int] -> Bool) -> a) -> [a]
makeTests tester = [ prop tester | prop <- propertyTesters ]

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) = if (x == y) then uniq (x:xs) else [x] ++ uniq (y:xs)

treeIsSorted :: (Eq a, Ord a) => RBTree a -> Bool
treeIsSorted tree = (toList tree) == (uniq $ sort $ toList tree)

rootIsBlack :: RBTree a -> Bool
rootIsBlack = isBlack

treesAreSorted :: Tester
treesAreSorted a =  a "All trees are sorted" $ \val -> treeIsSorted $ addAll Nil val

countOfBlackChildrenIsEqual :: (Show a) => RBTree a -> Bool
countOfBlackChildrenIsEqual tree = countBlackChildren tree >= 0

containsNoAdjacentRedNodes :: RBTree a -> Bool
containsNoAdjacentRedNodes tree = not $ anyTree childAndParentIsRed tree

makeTreeAIsBlack :: Tester
makeTreeAIsBlack a = a "makeTree a is black" $ \val ->
  (length val /= 1) || (makeTree (head val) :: RBTree Int) == Node Black (head val) Nil Nil

rootOfTreeIsAlwaysBlack :: Tester
rootOfTreeIsAlwaysBlack a = a "Root of tree is always black" $ \val -> rootIsBlack $ makeTree val

childIsRed :: RBTree a -> Bool
childIsRed Nil = False
childIsRed (Node _ _ l r)  = isRed l || isRed r

childAndParentIsRed :: RBTree a -> Bool
childAndParentIsRed tree = isRed tree && childIsRed tree

noAdjacentRedNodes :: Tester
noAdjacentRedNodes a = a "There are no adjacent red nodes" $ \val -> containsNoAdjacentRedNodes $ addAll Nil val

countBlackChildren :: (Show a) => RBTree a -> Int
countBlackChildren Nil = 1
countBlackChildren n@(Node c _ l r) = if (left == right) then left+addToCount else error ("Unbalanced left and right black children " ++ show n)
                                    where left = countBlackChildren l
                                          right = countBlackChildren r
                                          addToCount = if (c == Black) then 1 else 0

redBlackProperties :: (Ord a, Show a) => [RBTree a -> Bool]
redBlackProperties = [treeIsSorted, containsNoAdjacentRedNodes, countOfBlackChildrenIsEqual]

countOfBlackChildrenIsEqualForAllTrees :: Tester
countOfBlackChildrenIsEqualForAllTrees a = a "Count of black children is always equal" $ \val -> countOfBlackChildrenIsEqual $ addAll Nil val

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

deleteSome :: [Int] -> Bool
deleteSome a@(x:xs) = all (Lib.exists tree) (filter (/= x) xs) && (not (Lib.exists tree x))
    where fullTree = addAll Nil a
          tree = Lib.delete x fullTree
deleteSome _ = error ""

deleteSimple :: Tester
deleteSimple a = a "Deleting from tree" $ \val -> val == [] || deleteSome val

validateTree :: (Ord a, Show a) => RBTree a -> Bool
validateTree tree = all id $ map (\prop -> prop tree) redBlackProperties

treeIsValidAfterAnyOperation :: ([Char] -> ([Operation Int] -> Bool) -> t) -> t
treeIsValidAfterAnyOperation a = a "Tree is valid after any operation" (\operations -> validateTree $ operate Nil operations)

propertyTesters :: [([Char] -> ([Int] -> Bool) -> t) -> t]
propertyTesters = [treesAreSorted, makeTreeAIsBlack, rootOfTreeIsAlwaysBlack,
                   noAdjacentRedNodes, countOfBlackChildrenIsEqualForAllTrees,
                   zipperLeft, zipperRight, searchWorks, deleteSimple]

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
      addAll (makeIntTree 4) [1, 2, 3, 4, 5] @?= Node Black 4 (Node Black 2 (Node Red 1 Nil Nil) (Node Red 3 Nil Nil)) (Node Black 5 Nil Nil)
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
    , testCase "Validate double black delete" $
      validateTree (Lib.delete (0 :: Int) (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))) @?= True

    , testCase "Removed Red" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= takeLeft <&> postRemoveRotation Red <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Red" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= takeRight <&> postRemoveRotation Red <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))

    , testCase "Removed Black with Red replacement" $
      (Just (Node Black 1 (Node Red 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Black with Red replacement" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Red 2 Nil Nil)) <&> zipper >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))

    , testCase "Removed Black with Red nieces" $
      (Just (Node Black 0 Nil (Node Black 2 (Node Red 1 Nil Nil) (Node Red 3 Nil Nil))) <&> zipper >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Black 3 Nil Nil))
    , testCase "Removed Black with Red nieces" $
      (Just (Node Black 0 (Node Black (-2) (Node Red (-3) Nil Nil) (Node Red (-1) Nil Nil)) Nil) <&> zipper >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black (-2) (Node Black (-3) Nil Nil) (Node Black 0 (Node Red (-1) Nil Nil) Nil))

    , testCase "Removed Black with Red nieces" $
      (Just (Node Black 0 Nil (Node Black 2 (Node Red 1 Nil Nil) Nil)) <&> zipper >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Black with Red nieces" $
      (Just (Node Black 0 (Node Black (-2) Nil (Node Red (-1) Nil Nil)) Nil) <&> zipper >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black (-1) (Node Black (-2) Nil Nil) (Node Black 0 Nil Nil))

    , testCase "Removed Black with Black nieces" $
      (Just (Node Black 0 Nil (Node Black 1 Nil Nil)) <&> zipper >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 0 Nil (Node Red 1 Nil Nil))
    , testCase "Removed Black with Black nieces" $
      (Just (Node Black 0 (Node Black (-1) Nil Nil) Nil) <&> zipper >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 0 (Node Red (-1) Nil Nil) Nil)
    , testCase "Removed Black with Black nieces" $
      (Just (Node Black 2 (Node Black 0 Nil (Node Black 1 Nil Nil)) (Node Black 4 (Node Black 3 Nil Nil) (Node Black 5 Nil Nil))) <&> zipper >>= takeLeft >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Red 4 (Node Black 3 Nil Nil) (Node Black 5 Nil Nil)))
    , testCase "Removed Black with Black nieces" $
      (Just (Node Black 2 (Node Black 0 (Node Black (-1) Nil Nil) (Node Black 1 Nil Nil)) (Node Black 4 (Node Black 3 Nil Nil) Nil)) <&> zipper >>= takeRight >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Red 0 (Node Black (-1) Nil Nil) (Node Black 1 Nil Nil)) (Node Black 4 (Node Red 3 Nil Nil) Nil))
    , testCase "Removed Black with Black nieces" $
      (Just (Node Black 9 (Node Red 7 (Node Black 3 Nil (Node Red 5 Nil Nil)) Nil) (Node Black 10 Nil Nil)) <&> zipper >>= takeLeft >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 9 (Node Red 5 (Node Black 3 Nil Nil) (Node Black 7 Nil Nil)) (Node Black 10 Nil Nil))

    , testCase "Removed Black with Red sibling" $
      (Just (Node Black 0 Nil (Node Red 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil))) <&> zipper >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Black 3 Nil Nil))
    , testCase "Removed Black with Red sibling" $
      (Just (Node Black 5 (Node Black 1 Nil Nil) (Node Red 7 Nil (Node Black 8 Nil (Node Red 10 Nil Nil)))) <&> zipper >>= takeRight >>= takeLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 5 (Node Black 1 Nil Nil) (Node Red 8 (Node Black 7 Nil Nil) (Node Black 10 Nil Nil)))
    , testCase "Removed Black with Red sibling" $
      (Just (Node Black 10 (Node Red 2 (Node Black 1 Nil Nil) (Node Black 6 Nil (Node Red 8 Nil Nil))) Nil) <&> zipper >>= takeRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 1 Nil Nil) (Node Red 8 (Node Black 6 Nil Nil) (Node Black 10 Nil Nil)))

  ]
