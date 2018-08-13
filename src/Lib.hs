module Lib where

import Data.Maybe (fromJust, isJust)
import Data.Functor ((<&>))

data Color = Black | Red
             deriving (Eq, Show)
data RBTree a = Nil
              | Node Color a (RBTree a) (RBTree a)
              deriving (Eq, Show)

-- basic query functions

makeTree :: a -> RBTree a
makeTree a = Node Black a Nil Nil

makeRed :: a -> RBTree a
makeRed a = Node Red a Nil Nil

search :: (Ord a) => RBTree a -> a -> Maybe a
search Nil _ = Nothing
search (Node _ v l r) needle = case (compare needle v) of
    LT -> search l needle
    EQ -> Just v
    GT -> search r needle

exists :: (Ord a) => RBTree a -> a -> Bool
exists haystack needle = isJust $ search haystack needle

getNodeValue :: RBTree a -> Maybe a
getNodeValue Nil = Nothing
getNodeValue (Node _ val _ _) = Just val

getLeft :: RBTree a -> Maybe (RBTree a)
getLeft Nil = Nothing
getLeft (Node _ _ l _) = Just l

getRight :: RBTree a -> Maybe (RBTree a)
getRight Nil = Nothing
getRight (Node _ _ _ r) = Just r

-- simple iterating

mapNode :: (a -> RBTree a -> RBTree a -> b) -> RBTree a -> b
mapNode _ Nil = error "Cannot map a NIL"
mapNode f (Node _ v l r) = f v l r

walkTree :: (Ord b) => RBTree a -> (a -> RBTree b) -> RBTree b
walkTree Nil _ = Nil
walkTree node f = mapNode (\v l r -> flatten [f v, walkTree l f, walkTree r f]) node

anyTree :: (RBTree a -> Bool) -> RBTree a -> Bool
anyTree _ Nil = False
anyTree f node@(Node _ _ l r) = f node || anyTree f l || anyTree f r

allTree :: (RBTree a -> Bool) -> RBTree a -> Bool
allTree f tree = not $ anyTree (not . f) tree

flatten :: (Foldable t, Ord a) => t (RBTree a) -> RBTree a
flatten treeSet = foldr addAll Nil treeSet

-- color manipulation

isBlack :: RBTree a -> Bool
isBlack (Node Black _ _ _) = True
isBlack Nil = True
isBlack _ = False

isRed :: RBTree a -> Bool
isRed (Node Red _ _ _) = True
isRed _ = False

toBlack :: RBTree a -> RBTree a
toBlack (Node _ x l r) = Node Black x l r
toBlack Nil = Nil

toRed :: RBTree a -> RBTree a
toRed (Node _ x l r) = Node Red x l r
toRed Nil = Nil

-- zipper

type Zipper a = (Thread a, RBTree a)
data Branch a = RBLeft Color a (RBTree a)
              | RBRight Color a (RBTree a)
              deriving (Eq, Show)
type Thread a = [Branch a]

zipper :: RBTree a -> Zipper a
zipper t = ([], t)

unzipper :: Zipper a -> RBTree a
unzipper ([], t) = t
unzipper (RBLeft c x r:t, l) = unzipper (t, Node c x l r)
unzipper (RBRight c x l:t, r) = unzipper (t, Node c x l r)

takeLeft :: Zipper a -> Maybe (Zipper a)
takeLeft (t, Node c v l r) = Just (RBLeft c v r:t, l)
takeLeft _ = Nothing

takeRight :: Zipper a -> Maybe (Zipper a)
takeRight (t, Node c v l r) = Just (RBRight c v l:t, r)
takeRight _ = Nothing

back :: Zipper a -> Maybe (Zipper a)
back ([], _) = Nothing
back (RBLeft c x r:t, l) = Just (t, Node c x l r)
back (RBRight c x l:t, r) = Just (t, Node c x l r)

current :: Zipper a -> RBTree a
current (_, node) = node

sibling :: Zipper a -> Maybe (RBTree a)
sibling ([], _) = Nothing
sibling (RBLeft _ _ r:_, _) = Just r
sibling (RBRight _ _ l:_, _) = Just l

-- modifying zippers

rotateLeft :: Zipper a -> Maybe (Zipper a)
rotateLeft (t, Node c v l (Node rc rv rl rr)) = Just (t, Node rc rv (Node c v l rl) rr)
rotateLeft _ = Nothing

rotateRight :: Zipper a -> Maybe (Zipper a)
rotateRight (t, Node c v (Node lc lv ll lr) r) = Just (t, Node lc lv ll (Node c v lr r))
rotateRight _ = Nothing

swapColorWithParent :: Zipper a -> Maybe (Zipper a)
swapColorWithParent (((RBLeft pc pa pn):t), (Node c x l r)) = Just (((RBLeft c pa pn):t), (Node pc x l r))
swapColorWithParent (((RBRight pc pa pn):t), (Node c x l r)) = Just (((RBRight c pa pn):t), (Node pc x l r))
swapColorWithParent _ = Nothing

currentToRedAndChildrenToBlack :: Zipper a -> Maybe (Zipper a)
currentToRedAndChildrenToBlack (_, Nil) = Nothing
currentToRedAndChildrenToBlack (t, Node _ v l r) = Just (t, Node Red v (toBlack l) (toBlack r))

-- adding

add :: (Ord a) => a -> RBTree a -> RBTree a
add a Nil = makeTree a
add a tree = unzipper $ addToZipper a $ zipper tree

addAll :: (Ord a, Foldable t) => RBTree a -> t a -> RBTree a
addAll tree coll = foldr add tree coll

addToZipper :: (Ord a) => a -> Zipper a -> Zipper a
addToZipper value z@(thread, tree) =
  case getNodeValue tree <&> (compare value) of
    Nothing -> fromJust $ postAddRotation (thread, makeRed value)
    (Just LT) -> addToZipper value (fromJust $ takeLeft z)
    (Just EQ) -> addToZipper value (fromJust $ takeRight z) -- for a tree without duplicates, change this case to 'z'
    (Just GT) -> addToZipper value (fromJust $ takeRight z)

handleRedUncle :: (Ord a) => Zipper a -> Maybe (Zipper a)
handleRedUncle child = back child >>= back >>= currentToRedAndChildrenToBlack >>= postAddRotation

handleBlackUncle :: Zipper a -> Maybe (Zipper a)
handleBlackUncle z@(RBLeft{}:RBLeft{}:_, _)   = back z >>= swapColorWithParent >>= back >>= rotateRight
handleBlackUncle z@(RBRight{}:RBRight{}:_, _) = back z >>= swapColorWithParent >>= back >>= rotateLeft
handleBlackUncle z@(RBLeft{}:RBRight{}:_, _)  = back z >>= rotateRight >>= takeRight >>= handleBlackUncle
handleBlackUncle z@(RBRight{}:RBLeft{}:_, _)  = back z >>= rotateLeft >>= takeLeft >>= handleBlackUncle
handleBlackUncle _ = Nothing

postAddRotation :: (Ord a) => Zipper a -> Maybe (Zipper a)
postAddRotation z@(t, n) =
    case (isParentBlack, isUncleBlack) of
        (Nothing, _) -> Just (t, toBlack n)
        (Just True, _) -> Just z
        (Just _, Just True) -> handleBlackUncle z
        (Just _, Just False) -> handleRedUncle z
        (Just False, Nothing) -> error "Parent cannot be red if it has no siblings (violates 3 or 4)"
    where isParentBlack = back z <&> current <&> isBlack
          isUncleBlack = back z >>= sibling <&> isBlack

-- deletion

delete :: (Ord a) => a -> RBTree a -> RBTree a
delete a Nil = Nil
delete a tree = unzipper $ removeFromZipper a $ zipper tree

postRemoveRotation = Just . id

removeFromZipper :: (Ord a) => a -> Zipper a -> Zipper a
removeFromZipper value z@(thread, tree) =
  case getNodeValue tree <&> (compare value) of
    Nothing -> fromJust $ postRemoveRotation (thread, Nil)
    (Just LT) -> removeFromZipper value (fromJust $ takeLeft z)
    (Just EQ) -> removeFromZipper value (fromJust $ takeRight z) -- for a tree without duplicates, change this case to 'z'
    (Just GT) -> removeFromZipper value (fromJust $ takeRight z)

-- instances

instance Foldable RBTree where
   foldMap _ Nil = mempty
   foldMap f (Node _ k l r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Ord a => Semigroup (RBTree a) where
    (<>) = mappend

instance Ord a => Monoid (RBTree a) where
    mempty = Nil
    mappend = addAll
