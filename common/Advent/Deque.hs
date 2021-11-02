module Advent.Deque
  ( Deque(Empty,(:<|),(:|>))
  , (<|), (|>), singleton
  , fromList, appendList
  ) where

import Data.Bits (shiftR,(.&.))

import Debug.Trace

-- | Okasaki purely functional double ended queue
data Deque a = D [a] !Int [a] !Int
  deriving (Show)

pattern Empty <- D [] _ [] _
  where
    Empty = D [] 0 [] 0

pattern (:<|) :: a -> Deque a -> Deque a
pattern x :<| xs <- (popFront -> Just (x,xs))

pattern (:|>) :: Deque a -> a -> Deque a
pattern xs :|> x <- (popBack  -> Just (x,xs))

-- when one list becomes empty we split the other and reverse one half
mkDeque :: Deque a -> Deque a

mkDeque Empty = Empty

mkDeque (D [] 0 b bl) | bl > 1 = D f' fl' b' bl'
  where
    (b',reverse -> f') = splitAt half b
    half = bl `shiftR` 1
    rest = half + (bl .&. 1)
    bl' = half
    fl' = rest

mkDeque (D f fl [] 0) | fl > 1 = D f' fl' b' bl'
  where
    (f',reverse -> b') = splitAt half f
    half = fl `shiftR` 1
    rest = half + (fl .&. 1)
    fl' = half
    bl' = rest

mkDeque d = d

(<|) :: a -> Deque a -> Deque a
x <| (D f fl b bl) = mkDeque (D (x:f) (fl+1) b bl)
infixr <|

(|>) :: Deque a -> a -> Deque a
(D f fl b bl) |> x = mkDeque (D f fl (x:b) (bl+1))
infixl |>

popFront, popBack :: Deque a -> Maybe (a,Deque a)

popFront (D (x:xs) fl b bl) = Just (x,mkDeque (D xs (fl-1) b bl))
popFront (D _ 0 [x] 1)      = Just (x,Empty)
popFront Empty              = Nothing

popBack  (D f fl (x:xs) bl) = Just (x,mkDeque (D f fl xs (bl-1)))
popBack  (D [x] 1 _ 0)      = Just (x,Empty)
popBack  Empty              = Nothing

singleton :: a -> Deque a
singleton x = D [x] 1 [] 0

fromList :: [a] -> Deque a
fromList xs = mkDeque (D xs (length xs) [] 0)

appendList :: [a] -> Deque a -> Deque a
appendList xs d = foldl (|>) d xs
