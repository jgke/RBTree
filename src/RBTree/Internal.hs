{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module RBTree.Internal where

import Data.Maybe (fromJust, isJust)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))

data Color = Black | Red
             deriving (Eq, Show)
data RBTree a = Nil
              | Node Color a !(RBTree a) !(RBTree a)
              deriving (Eq, Show)

class Colorable a where
  getColor :: a -> Color
  setColor :: Color -> a -> a

instance Colorable (RBTree a) where
  getColor (Node c _ _ _) = c
  getColor Nil = Black
  setColor c (Node _ v l r) = (Node c v l r)
  setColor _ Nil = Nil

instance Foldable RBTree where
   foldMap _ Nil = mempty
   foldMap f (Node _ k l r) = foldMap f l `mappend` f k `mappend` foldMap f r

instance Ord a => Semigroup (RBTree a) where
    (<>) = mappend

instance Ord a => Monoid (RBTree a) where
    mempty = Nil
    mappend = addAll

emptyTree :: RBTree a
emptyTree = Nil

makeTree :: a -> RBTree a
makeTree a = Node Black a Nil Nil

-- basic query functions

search :: (Ord a) => RBTree a -> a -> Maybe a
search Nil _ = Nothing
search (Node _ v l r) needle = case (compare needle v) of
    LT -> search l needle
    EQ -> Just v
    GT -> search r needle

exists :: (Ord a) => RBTree a -> a -> Bool
exists haystack needle = isJust $ search haystack needle

size :: RBTree a -> Int
size Nil = 0
size (Node _ _ l r) = 1 + (size l) + (size r)

getNodeValue :: RBTree a -> Maybe a
getNodeValue Nil = Nothing
getNodeValue (Node _ val _ _) = Just val

isBlack :: (Colorable a) => a -> Bool
isBlack c = getColor c == Black

isRed :: (Colorable a) => a -> Bool
isRed c = getColor c == Red

-- node manipulation

setValue :: a -> RBTree a -> RBTree a
setValue nx (Node c _ l r) = Node c nx l r
setValue _ Nil = Nil

toBlack :: (Colorable a) => a -> a
toBlack = setColor Black

toRed :: (Colorable a) => a -> a
toRed = setColor Red

-- zipper

type Zipper a = (Thread a, RBTree a)
data Branch a = RBLeft Color a (RBTree a)
              | RBRight Color a (RBTree a)
              deriving (Eq, Show)
type Thread a = [Branch a]

instance Colorable (Zipper a) where
    getColor = getColor . snd
    setColor c (t, n) = (t, setColor c n)

zipper :: RBTree a -> Zipper a
zipper t = ([], t)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (t, Node c v l r) = Just (RBLeft c v r:t, l)
goLeft _ = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (t, Node c v l r) = Just (RBRight c v l:t, r)
goRight _ = Nothing

goBack :: Zipper a -> Maybe (Zipper a)
goBack ([], _) = Nothing
goBack (RBLeft c x r:t, l) = Just (t, Node c x l r)
goBack (RBRight c x l:t, r) = Just (t, Node c x l r)

unzipper :: Zipper a -> RBTree a
unzipper ([], t) = t
unzipper (RBLeft c x r:t, l) = unzipper (t, Node c x l r)
unzipper (RBRight c x l:t, r) = unzipper (t, Node c x l r)

sibling :: Zipper a -> Maybe (Zipper a)
sibling ([], _) = Nothing
sibling z@(RBLeft{}:_, _) = goBack z >>= goRight
sibling z@(RBRight{}:_, _) = goBack z >>= goLeft

zipTo :: (Ord a) => a -> Zipper a -> Either (Zipper a) (Zipper a)
zipTo value z@(_, tree) =
  case getNodeValue tree <&> compare value of
    Nothing -> Left z
    (Just LT) -> maybeToEither (goLeft z) (zipTo value) z
    (Just EQ) -> Right z
    (Just GT) -> maybeToEither (goRight z) (zipTo value) z

-- modifying zippers

rotateLeft :: Zipper a -> Maybe (Zipper a)
rotateLeft (t, Node c v l (Node rc rv rl rr)) = Just (t, Node rc rv (Node c v l rl) rr)
rotateLeft _ = Nothing

rotateRight :: Zipper a -> Maybe (Zipper a)
rotateRight (t, Node c v (Node lc lv ll lr) r) = Just (t, Node lc lv ll (Node c v lr r))
rotateRight _ = Nothing

swapColorWithParent :: Zipper a -> Maybe (Zipper a)
swapColorWithParent (RBLeft pc pa pn:t, Node c x l r) = Just (RBLeft c pa pn:t, Node pc x l r)
swapColorWithParent (RBRight pc pa pn:t, Node c x l r) = Just (RBRight c pa pn:t, Node pc x l r)
swapColorWithParent _ = Nothing

swapValueWithParent :: Zipper a -> Maybe (Zipper a)
swapValueWithParent (RBLeft pc pa pn:t, Node c x l r) = Just (RBLeft pc x pn:t, Node c pa l r)
swapValueWithParent (RBRight pc pa pn:t, Node c x l r) = Just (RBRight pc x pn:t, Node c pa l r)
swapValueWithParent _ = Nothing

currentToRedAndChildrenToBlack :: Zipper a -> Maybe (Zipper a)
currentToRedAndChildrenToBlack (_, Nil) = Nothing
currentToRedAndChildrenToBlack (t, Node _ v l r) = Just (t, Node Red v (toBlack l) (toBlack r))

-- adding

add :: (Ord a) => a -> RBTree a -> RBTree a
add a Nil = (Node Black a Nil Nil)
add a tree = unzipper $ addToZipper a $ zipper tree

addAll :: (Ord a, Foldable t) => RBTree a -> t a -> RBTree a
addAll tree coll = foldr add tree coll

-- adding step 1: add to tree
addToZipper :: (Ord a) => a -> Zipper a -> Zipper a
addToZipper value z@(thread, tree) =
  case (zipTo value z) of
    Right n -> mapSnd (setValue value) n --already present, replace existing
    Left z -> fromJust $ postAddRotation (fst z, (Node Red value Nil Nil))

-- adding step 2: re-balance tree
postAddRotation :: (Ord a) => Zipper a -> Maybe (Zipper a)
postAddRotation z@(t, n) =
    case (isParentBlack, isUncleBlack) of
        (Nothing, _) -> Just (t, toBlack n)
        (Just True, _) -> Just z
        (Just _, Just True) -> handleBlackUncle z
        (Just _, Just False) -> handleRedUncle z
        (Just False, Nothing) -> error "Parent cannot be red if it has no siblings (violates 3 or 4)"
    where isParentBlack = goBack z <&> isBlack
          isUncleBlack = goBack z >>= sibling <&> isBlack


--          Black
--        Red   Red
-- --> Red  c   d e
--     a b
--
--   -->  Red
--    Black Black
-- Red    c d   e
-- a b
handleRedUncle :: (Ord a) => Zipper a -> Maybe (Zipper a)
handleRedUncle child = goBack child >>= goBack >>= currentToRedAndChildrenToBlack >>= postAddRotation

handleBlackUncle :: Zipper a -> Maybe (Zipper a)

--          Black
--        Red   Black
-- --> Red  c   d   e
--     a b
--
-- --> Black
--  Red     Red
--  a b     c  Black
--             d   e
handleBlackUncle z@(RBLeft{}:RBLeft{}:_, _)   = goBack z >>= swapColorWithParent >>= goBack >>= rotateRight
handleBlackUncle z@(RBRight{}:RBRight{}:_, _) = goBack z >>= swapColorWithParent >>= goBack >>= rotateLeft

--        Black
--      Red   Black
--  --> a Red d   e
--        b c
--
-- --> Black
--  Red     Red
--  a b     c  Black
--             d   e
handleBlackUncle z@(RBRight{}:RBLeft{}:_, _)  = goBack z >>= rotateLeft >>= swapColorWithParent >>= goBack >>= rotateRight
handleBlackUncle z@(RBLeft{}:RBRight{}:_, _)  = goBack z >>= rotateRight >>= swapColorWithParent >>= goBack >>= rotateLeft
handleBlackUncle _ = Nothing

-- deletion

remove :: (Ord a) => a -> RBTree a -> RBTree a
remove a tree = unzipper $ removeFromZipper a $ zipper tree

-- deletion step 1: delete node
removeFromZipper :: (Ord a) => a -> Zipper a -> Zipper a
removeFromZipper value z =
  case (zipTo value z) of
    Left _ -> z -- not found, do nothing
    Right node -> (uncurry postRemoveRotation) $ dropNode node

dropNode :: (Ord a) => Zipper a -> (Color, Zipper a)
dropNode (t, Nil) = (Black, (t, Nil))
dropNode (t, Node c _ Nil r) = (c, (t, r))
dropNode (t, Node c _ l Nil) = (c, (t, l))
dropNode z@(_, Node{}) = fromJust $ swapValueWithSuccessor z <&> dropNode

swapValueWithSuccessor :: (Ord a) => Zipper a -> Maybe (Zipper a)
swapValueWithSuccessor (_, Nil) = Nothing
swapValueWithSuccessor (thread, (Node c v l r)) = do
    successor <- toSuccessor v $ zipper r
    successorValue <- getNodeValue $ snd successor
    let newParent = Node c successorValue l r
    newZipper <- goRight (thread, newParent)
    return ((fst successor)++(fst newZipper), setValue v $ snd successor)

toSuccessor :: (Ord a) => a -> Zipper a -> Maybe (Zipper a)
toSuccessor value z@(_, tree) =
  case getNodeValue tree <&> (compare value) of
    Nothing -> Nothing
    (Just GT) -> goRight z >>= toSuccessor value
    (Just EQ) -> goRight z >>= toSuccessor value
    (Just LT) -> (goLeft z >>= toSuccessor value) <|> Just z

-- deletion step 2: re-balance tree
postRemoveRotation :: Color -> Zipper a -> Zipper a
postRemoveRotation Red z = z
postRemoveRotation _ z@(_, Node Red _ _ _) = toBlack z
postRemoveRotation _ z@([], _) = toBlack z
postRemoveRotation _ z = case (sibling z <&> isRed) of
    Just True -> fromJust $ handleRedSibling z
    Just False -> fromJust $ handleBlackSibling z
    Nothing -> error "unreachable"

handleRedSibling :: Zipper a -> Maybe (Zipper a)
--         Black
-- --> Nil      Red
--          Black Black
--
--           Black
--       Red      Black
-- --> Nil Black
handleRedSibling z@(RBLeft{}:_, _) = goBack z >>= rotateLeft <&> toBlack >>= goLeft <&> toRed >>= goLeft <&> postRemoveRotation Black
handleRedSibling z@(RBRight{}:_, _) = goBack z >>= rotateRight <&> toBlack >>= goRight <&> toRed >>= goRight <&> postRemoveRotation Black
handleRedSibling _ = error ""

handleBlackSibling :: Zipper a -> Maybe (Zipper a)
handleBlackSibling z@(x:_, _) =
    case (x, isLeftNiblingRed, isRightNiblingRed) of

        --       Black
        -- --> Nil   Black
        --         Red   Red
        --
        --      Black
        -- Black     Black <--
        --     Red
        (RBLeft{}, _, Just True) -> goBack z >>= rotateLeft >>= goLeft >>= swapColorWithParent >>= sibling <&> toBlack
        (RBRight{}, Just True, _) -> goBack z >>= rotateRight >>= goRight >>= swapColorWithParent >>= sibling <&> toBlack

        --       Black
        -- --> Nil   Black
        --         Red
        --
        --          Black
        -- --> Black     Black
        (RBLeft{}, Just True, _) -> sibling z >>= rotateRight <&> toBlack >>= goBack >>= rotateLeft >>= goLeft >>= swapColorWithParent
        (RBRight{}, _, Just True) -> sibling z >>= rotateLeft <&> toBlack >>= goBack >>= rotateRight >>= goRight >>= swapColorWithParent

        --       Black
        -- --> Nil   Black
        --
        -- --> Black
        --   Nil   Red
        _ -> sibling z <&> toRed >>= goBack <&> postRemoveRotation Black
    where isLeftNiblingRed = sibling z >>= goLeft <&> isRed
          isRightNiblingRed = sibling z >>= goRight <&> isRed
handleBlackSibling _ = error ""

areChildrenRed :: Zipper a -> (Maybe Bool, Maybe Bool)
areChildrenRed z = (goLeft z <&> isRed, goRight z <&> isRed)

-- utils

maybeToEither :: Maybe a -> (a -> Either b c) -> b -> Either b c
maybeToEither Nothing _ b = Left b
maybeToEither (Just a) f _ = f a

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
