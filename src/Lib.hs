module Lib where

import Data.Maybe (fromJust)
import Debug.Trace

data Color = Black | Red
             deriving (Eq, Show)
data RBTree a = Nil
              | Node Color a (RBTree a) (RBTree a)
              deriving (Eq, Show)

instance Foldable RBTree where
   foldMap _ Nil = mempty
   foldMap f (Node _ k l r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Ord a => Semigroup (RBTree a) where
    (<>) = mappend

instance Ord a => Monoid (RBTree a) where
    mempty = Nil
    mappend = addAll

makeTree :: a -> RBTree a
makeTree a = Node Black a Nil Nil

makeRed :: a -> RBTree a
makeRed a = Node Red a Nil Nil

-- zipper
type Zipper a = (Thread a, RBTree a)
data Branch a = RBLeft Color a (RBTree a)
              | RBRight Color a (RBTree a)
              deriving (Eq, Show)
type Thread a = [Branch a]

zipper :: RBTree a -> Zipper a
zipper t = ([], t)

unZip :: Zipper a -> RBTree a
unZip ([], t) = t
unZip z = unZip (fromJust $ back z)

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

add :: (Ord a) => a -> RBTree a -> RBTree a
add a Nil = makeTree a
add a tree = unZip $ addZ a $ zipper tree

getValue :: RBTree a -> Maybe a
getValue Nil = Nothing
getValue (Node _ val _ _) = Just val

sibling :: Zipper a -> RBTree a
sibling ([], _) = error "No sibling without parents"
sibling (RBLeft _ _ r:_, _) = r
sibling (RBRight _ _ l:_, _) = l

toBlack :: RBTree a -> RBTree a
toBlack (Node _ x l r) = Node Black x l r
toBlack Nil = Nil


toRed :: RBTree a -> RBTree a
toRed (Node _ x l r) = Node Red x l r
toRed Nil = Nil

currentToRedAndChildrenToBlack :: Zipper a -> Zipper a
currentToRedAndChildrenToBlack (_, Nil) = error "No children for NIL"
currentToRedAndChildrenToBlack (t, Node _ v l r) = (t, Node Red v (toBlack l) (toBlack r))

handleRedUncle :: (Ord a) => Zipper a -> Zipper a
handleRedUncle child = postAddRotation $ currentToRedAndChildrenToBlack grandparent
  where grandparent = fromJust $ (Just child >>= back >>= back)

rotateLeft :: Zipper a -> Maybe (Zipper a)
rotateLeft (t, Node c v l (Node rc rv rl rr)) = Just (t, Node rc rv (Node c v l rl) rr)
rotateLeft _ = error ""

rotateRight :: Zipper a -> Maybe (Zipper a)
rotateRight (t, Node c v (Node lc lv ll lr) r) = Just (t, Node lc lv ll (Node c v lr r))
rotateRight _ = error ""

handleBlackUncle :: (Ord a) => Zipper a -> Zipper a
handleBlackUncle (((RBLeft pc pa pn):(RBLeft gpc gpa gcn):t), child) =
  fromJust $ Just ((RBLeft gpc pa pn):(RBLeft pc gpa gcn):t, child) >>= back >>= back >>= rotateRight
handleBlackUncle z@(((RBRight _ _ _):(RBLeft _ _ _):_), _) =
  handleBlackUncle $ fromJust $ Just z >>= back >>= rotateLeft >>= takeLeft
handleBlackUncle (((RBRight pc pa pn):(RBRight gpc gpa gcn):t), child) =
  fromJust $ Just ((RBRight gpc pa pn):(RBRight pc gpa gcn):t, child) >>= back >>= back >>= rotateLeft
handleBlackUncle z@(((RBLeft _ _ _):(RBRight _ _ _):_), _) =
  handleBlackUncle $ fromJust $ Just z >>= back >>= rotateRight >>= takeRight
handleBlackUncle _ = error "No black uncle or inconsistent state"

postAddRotation :: (Ord a) => Zipper a -> Zipper a
postAddRotation z@(t, n) = case (back z) of
  Nothing -> (t, toBlack n)
  Just parent -> if (isBlack $ snd parent)
    then z
    else if (isBlack $ sibling parent)
         then handleBlackUncle z
         else handleRedUncle z

addZ :: (Ord a) => a -> Zipper a -> Zipper a
addZ a z@(thread, tree) =
  let val = getValue tree
  in case val of
    Nothing -> postAddRotation (thread, makeRed a)
    (Just value) -> if (a < value)
      then addZ a (fromJust $ takeLeft z)
      else addZ a (fromJust $ takeRight z)

getLeft :: RBTree a -> Maybe (RBTree a)
getLeft Nil = Nothing
getLeft (Node _ _ l _) = Just l

getRight :: RBTree a -> Maybe (RBTree a)
getRight Nil = Nothing
getRight (Node _ _ _ r) = Just r

addAll :: (Ord a, Foldable t) => RBTree a -> t a -> RBTree a
addAll tree coll = foldr add tree coll

isBlack :: RBTree a -> Bool
isBlack (Node Black _ _ _) = True
isBlack Nil = True
isBlack _ = False

isRed :: RBTree a -> Bool
isRed (Node Red _ _ _) = True
isRed _ = False

mapNode :: (a -> RBTree a -> RBTree a -> b) -> RBTree a -> b
mapNode f Nil = error "Cannot map a NIL"
mapNode f (Node _ v l r) = f v l r

-- >>= with Ord
walkTree :: (Ord b) => RBTree a -> (a -> RBTree b) -> RBTree b
walkTree Nil _ = Nil
walkTree node f = mapNode (\v l r -> flatten [f v, walkTree l f, walkTree r f]) node

anyTree :: (RBTree a -> Bool) -> RBTree a -> Bool
anyTree _ Nil = False
anyTree f node@(Node _ v l r) = f node || anyTree f l || anyTree f r

allTree :: (RBTree a -> Bool) -> RBTree a -> Bool
allTree f tree = not $ anyTree (not . f) tree

flatten :: (Foldable t, Ord a) => t (RBTree a) -> RBTree a
flatten treeSet = foldr addAll Nil treeSet
