module Main (main) where

import Advent.Input        (getInputLines)
import Data.Char           (isDigit)
import Data.List           qualified as L
import Control.Applicative ((<|>))

main =
  do inp <- getInputLines parse 18
     print (part1 inp)
     print (part2 inp)

-- read and show

parse = read @(Tree Int) . concatMap expand
  where
    expand '[' = "Node ["
    expand c | isDigit c = "Leaf " ++ [c]
    expand c = [c]

data Tree a = Leaf a | Node [Tree a]
  deriving Read

instance Show a => Show (Tree a) where
  showsPrec _ (Leaf n) = shows n
  showsPrec _ (Node ts) = showList ts

-- zipper

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a) deriving Show

type Loc a = (Tree a,Cxt a)

top :: Tree a -> Loc a
top t = (t,Top)

left, right, up, upmost :: Loc a -> Loc a
left (Node [l,r],c) = (l, L c r)
right (Node [l,r],c) = (r, R l c)

up (t,L c r) = (Node [t,r],c)
up (t,R l c) = (Node [l,t],c)

upmost l@(_,Top) = l
upmost l = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t,c) f = (f t,c)

-- explosions

explode t =
  case go 0 (top t) of
    Nothing -> Nothing
    Just l@(Node [Leaf a,Leaf b],c)
            -> let l' = modify l (const (Leaf 0))
                   (t',Top) = upmost (patch (Just a) (Just b) l')
               in Just t'

  where

    go n l@(t,_) | n < 4 = case t of
      Leaf _ -> Nothing
      Node _ -> go (n+1) (left l) <|> go (n+1) (right l)

    go n (Leaf _,_) = Nothing
    go n l@(Node _,_) = Just l

-- leftmost regular number to the location l
lm :: Loc a -> Maybe (Loc a)
lm  l@(_,Top)    = Nothing
lm  l@(_,R _ _)  = lm (up l)
lm  l@(_,L _ _)  = lm' ((right . up) l)
lm' l@(Leaf _,_) = Just l
lm' l@(Node _,_) = lm' (left l)

-- rightmost regular number to the location l
rm :: Loc a -> Maybe (Loc a)
rm  l@(_,Top)    = Nothing
rm  l@(_,L _ _)  = rm (up l)
rm  l@(_,R _ _)  = rm' ((left . up) l)
rm' l@(Leaf _,_) = Just l
rm' l@(Node _,_) = rm' (right l)

patch Nothing  Nothing  l           = l

patch mx       (Just y) l@(_,L _ _) =
  patch mx Nothing $ up $ (left . up) $ modify ((right . up) l) insertLeft
  where
    insertLeft (Leaf n) = Leaf (n+y)
    insertLeft (Node [a,b]) = Node [insertLeft a,b]

patch (Just x) my       l@(_,R _ _) =
  patch Nothing my $ up $ (right . up) $ modify ((left . up) l) insertRight
  where
    insertRight (Leaf n) = Leaf (n+x)
    insertRight (Node [a,b]) = Node [a,insertRight b]

patch Nothing  (Just y) l@(_,R _ _) =
  case lm l of
    Nothing -> l
    Just l' -> modify l' (\(Leaf n) -> Leaf (n+y))

patch (Just x) Nothing  l@(_,L _ _) =
  case rm l of
    Nothing -> l
    Just l' -> modify l' (\(Leaf n) -> Leaf (n+x))

-- splits

split :: Tree Int -> Maybe (Tree Int)
split t = mt'
  where
    mt' | Just l' <- go (top t)
        , (t',_) <- upmost l'   = Just t'
        | otherwise             = Nothing

    go l@(Node _,_) = go (left l) <|> go (right l)
    go l@(Leaf n,_)
      | n >= 10   = Just $ modify l (\_ -> Node [Leaf down,Leaf up])
      | otherwise = Nothing
      where
        down = n `div` 2
        up = down + (n `mod` 2)

-- additions

add t1 t2 = go begin
  where
    begin = Node [t1,t2]

    go t
      | Just t1 <- explode t = go t1
      | Just t1 <- split t   = go t1
      | otherwise            = t

-- magnitude

magnitude (Leaf n) = n
magnitude (Node [a,b]) = 3 * magnitude a + 2 * magnitude b

-- answers

part1 = magnitude . L.foldl1 add

part2 xs = maximum [ magnitude (add x y) | (x,ys) <- select xs, y <- ys ]

select xs = [ (x, l++r) | (l,x:r) <- zip (L.inits xs) (L.tails xs) ]
