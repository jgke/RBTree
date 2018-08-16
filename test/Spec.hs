{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series as SCSeries
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Foldable
import Data.Maybe (isJust)
import Data.Functor

import RBTree
import RBTree.Internal

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
operate tree (Delete item:xs) = operate (remove item tree) xs

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
rootOfTreeIsAlwaysBlack a = a "Root of tree is always black" $ \val -> isBlack $ addAll Nil val

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
zipperLawsLeft tree = ((Just . zipper) tree >>= goLeft >>= goBack >>= (Just . snd)) == Just tree

zipperLawsRight :: (Ord a) => RBTree a -> Bool
zipperLawsRight tree = ((Just . zipper) tree >>= goLeft >>= goBack >>= (Just . snd)) == Just tree

type Tester = forall t. ([Char] -> ([Int] -> Bool) -> t) -> t

zipperLeft :: Tester
zipperLeft a = a "goLeft . goBack = id" $ \val -> val == [] || zipperLawsLeft (addAll Nil (val :: [Int]))

zipperRight :: Tester
zipperRight a = a "goRight . goBack = id" $ \val -> val == [] || zipperLawsRight (addAll Nil (val :: [Int]))

searchSome :: [Int] -> Bool
searchSome (x:xs) = (not $ elem x xs) || (isJust $ search (addAll Nil xs) x)
searchSome _ = error ""

searchWorks :: Tester
searchWorks a = a "Searching tree" $ \val -> val == [] || searchSome val

deleteSome :: [Int] -> Bool
deleteSome a@(x:xs) = all (RBTree.exists tree) (filter (/= x) xs) && (not (RBTree.exists tree x))
    where fullTree = addAll Nil a
          tree = remove x fullTree
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
      ((Just $ zipper (Nil :: RBTree Int)) >>= goLeft) @?= Nothing
    , testCase "Navigating one tree" $
      ((Just $ zipper (makeIntTree 0)) >>= goLeft >>= (Just . snd)) @?= Just Nil
    , testCase "Navigating goBack and forth" $
      ((Just $ zipper (makeIntTree 0)) >>= goLeft >>= goBack >>= (Just . snd)) @?= Just (makeTree 0)
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
      validateTree (remove (0 :: Int) (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))) @?= True

    , testCase "bemoved Red 1" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= goLeft <&> postRemoveRotation Red <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Red 2" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= goRight <&> postRemoveRotation Red <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))

    , testCase "Removed Black with Red replacement 1" $
      (Just (Node Black 1 (Node Red 0 Nil Nil) (Node Black 2 Nil Nil)) <&> zipper >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Black with Red replacement 2" $
      (Just (Node Black 1 (Node Black 0 Nil Nil) (Node Red 2 Nil Nil)) <&> zipper >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))

    , testCase "Removed Black with Red nieces 1" $
      (Just (Node Black 0 Nil (Node Black 2 (Node Red 1 Nil Nil) (Node Red 3 Nil Nil))) <&> zipper >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Black 3 Nil Nil))
    , testCase "Removed Black with Red nieces 2" $
      (Just (Node Black 0 (Node Black (-2) (Node Red (-3) Nil Nil) (Node Red (-1) Nil Nil)) Nil) <&> zipper >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black (-2) (Node Black (-3) Nil Nil) (Node Black 0 (Node Red (-1) Nil Nil) Nil))

    , testCase "Removed Black with Red nieces 3" $
      (Just (Node Black 0 Nil (Node Black 2 (Node Red 1 Nil Nil) Nil)) <&> zipper >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 1 (Node Black 0 Nil Nil) (Node Black 2 Nil Nil))
    , testCase "Removed Black with Red nieces 4" $
      (Just (Node Black 0 (Node Black (-2) Nil (Node Red (-1) Nil Nil)) Nil) <&> zipper >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black (-1) (Node Black (-2) Nil Nil) (Node Black 0 Nil Nil))

    , testCase "Removed Black with Black nieces 1" $
      (Just (Node Black 0 Nil (Node Black 1 Nil Nil)) <&> zipper >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 0 Nil (Node Red 1 Nil Nil))
    , testCase "Removed Black with Black nieces 2" $
      (Just (Node Black 0 (Node Black (-1) Nil Nil) Nil) <&> zipper >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 0 (Node Red (-1) Nil Nil) Nil)
    , testCase "Removed Black with Black nieces 3" $
      (Just (Node Black 2 (Node Black 0 Nil (Node Black 1 Nil Nil)) (Node Black 4 (Node Black 3 Nil Nil) (Node Black 5 Nil Nil))) <&> zipper >>= goLeft >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Red 4 (Node Black 3 Nil Nil) (Node Black 5 Nil Nil)))
    , testCase "Removed Black with Black nieces 4" $
      (Just (Node Black 2 (Node Black 0 (Node Black (-1) Nil Nil) (Node Black 1 Nil Nil)) (Node Black 4 (Node Black 3 Nil Nil) Nil)) <&> zipper >>= goRight >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Red 0 (Node Black (-1) Nil Nil) (Node Black 1 Nil Nil)) (Node Black 4 (Node Red 3 Nil Nil) Nil))
    , testCase "Removed Black with Black nieces 5" $
      (Just (Node Black 9 (Node Red 7 (Node Black 3 Nil (Node Red 5 Nil Nil)) Nil) (Node Black 10 Nil Nil)) <&> zipper >>= goLeft >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 9 (Node Red 5 (Node Black 3 Nil Nil) (Node Black 7 Nil Nil)) (Node Black 10 Nil Nil))

    , testCase "Removed Black with Red sibling 1" $
      (Just (Node Black 0 Nil (Node Red 2 (Node Black 1 Nil Nil) (Node Black 3 Nil Nil))) <&> zipper >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 0 Nil (Node Red 1 Nil Nil)) (Node Black 3 Nil Nil))
    , testCase "Removed Black with Red sibling 2" $
      (Just (Node Black 5 (Node Black 1 Nil Nil) (Node Red 7 Nil (Node Black 8 Nil (Node Red 10 Nil Nil)))) <&> zipper >>= goRight >>= goLeft <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 5 (Node Black 1 Nil Nil) (Node Red 8 (Node Black 7 Nil Nil) (Node Black 10 Nil Nil)))
    , testCase "Removed Black with Red sibling 3" $
      (Just (Node Black 10 (Node Red 2 (Node Black 1 Nil Nil) (Node Black 6 Nil (Node Red 8 Nil Nil))) Nil) <&> zipper >>= goRight <&> postRemoveRotation Black <&> unzipper :: Maybe (RBTree Int))
            @?= Just (Node Black 2 (Node Black 1 Nil Nil) (Node Red 8 (Node Black 6 Nil Nil) (Node Black 10 Nil Nil)))

  ]
