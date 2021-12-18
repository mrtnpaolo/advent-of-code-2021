module Main (main) where

import Advent.Input
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Control.Applicative
import Debug.Trace

main =
  do inp <- getInputLines parse 18
     mapM_ print inp
     putStrLn ""
     part1 inp
     print (part2 inp)

parse = read @(Tree Int) . expand
  where
    expand = concatMap f
    f '[' = "Node ["
    f c | isDigit c = "Leaf " ++ [c]
    f c = [c]

data Tree a = Leaf a | Node [Tree a]
  deriving (Read,Functor)

instance Show a => Show (Tree a) where
  showsPrec _ (Leaf n) = shows n
  showsPrec _ (Node ts) = showList ts

data Cxt a = Top | L (Cxt a) (Tree a) | R (Tree a) (Cxt a) deriving Show

type Loc a = (Tree a,Cxt a)

top :: Tree a -> Loc a
top t = (t,Top)

isTop :: Loc a -> Bool
isTop (_,Top) = True
isTop _       = False

left, right, up, upmost :: Loc a -> Loc a
left (Node [l,r],c) = (l, L c r)
right (Node [l,r],c) = (r, R l c)

up (t,L c r) = (Node [t,r],c)
up (t,R l c) = (Node [l,t],c)

upmost l@(_,Top) = l
upmost l = upmost (up l)

modify :: Loc a -> (Tree a -> Tree a) -> Loc a
modify (t,c) f = (f t,c)

-- if any pair is nested inside four pairs
--   the leftmost such pair explodes
-- if any regular number is 10 or greater
--   the leftmost such regular number splits


explode t =
  case go 0 (top t) of
    Nothing -> Nothing
    Just l@(Node [Leaf a,Leaf b],c)
            -> let l' = modify l (const (Leaf 0))
                   (t',Top) = upmost (patch (Just a) (Just b) l')
                   --t' = patch (Just a) (Just b) l'
               in Just $ t'

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

patch Nothing  Nothing  l
  = l

patch mx       (Just y) l@(_,L _ _) =
  patch mx Nothing $ up $ (left . up) $ modify ((right . up) l) f -- fmap (y+))
  where
    f (Leaf n) = Leaf (n+y)
    f (Node [a,b]) = Node [f a,b]

patch (Just x) my       l@(_,R _ _) =
  patch Nothing my $ up $ (right . up) $ modify ((left . up) l) f -- fmap (x+))
  where
    f (Leaf n) = Leaf (n+x)
    f (Node [a,b]) = Node [a,f b]

patch mx my l@(_,Top) = l

patch Nothing (Just y) l@(_,R _ _) =
  case lm l of
    Nothing -> l
    Just l' -> modify l' (\(Leaf n) -> Leaf (n+y))

patch (Just x) Nothing l@(_,L _ _) =
  case rm l of
    Nothing -> l
    Just l' -> modify l' (\(Leaf n) -> Leaf (n+x))

insertLeft n l@(Leaf m,c) = (Leaf (m+n),c)
insertLeft n l@(Node _,_) = insertLeft n (left l)

insertRight n l@(Leaf m,c) = (Leaf (m+n),c)
insertRight n l@(Node _,_) = insertRight n (right l)


te = mapM_ (\(a,b) -> print a >> print b >> putStrLn "") $
  map (\xs -> let t = parse xs in (t,explode t)) $
  [ "[[[[[9,8],1],2],3],4]"
  , "[7,[6,[5,[4,[3,2]]]]]"
  , "[[6,[5,[4,[3,2]]]],1]"
  , "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
  , "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
  ]

-- splits

split :: Tree Int -> Maybe (Tree Int)
split t = mt'
  where
    mt' | Just l' <- go (top t)
        , (t',_) <- upmost l'   = Just t'
        | otherwise             = Nothing

    go l@(Node _,_) = go (left l) <|> go (right l)
    go l@(Leaf n,_)
      | n >= 10 = Just $ modify l (\_ -> let down = n `div` 2
                                             up = down + (n `mod` 2)
                                         in Node [Leaf down,Leaf up])
      | otherwise = Nothing

ts = mapM_ (\(a,b) -> print a >> print b >> putStrLn "") $
  map (\xs -> let t = xs in (t,split t)) $
  [ Leaf 10
  , Leaf 11
  , Leaf 12
  , Leaf 0
  ]

-- additions

add t1 t2 = end
  where
    begin = Node [t1,t2]

    end = go begin

    go t
      | Just t1 <- explode t = go t1 -- (traceShowId t1)
      | Just t1 <- split t   = go t1 -- (traceShowId t1)
      | otherwise            = t

t = mapM_  print $
  map (\(x,y) -> let t1 = parse x; t2 = parse y in add t1 t2) $
  [ ("[[[[4,3],4],4],[7,[[8,4],9]]]","[1,1]")
  ]

-- magnitude

magnitude (Leaf n) = n
magnitude (Node [a,b]) = 3 * magnitude a + 2 * magnitude b

part1 :: [Tree Int] -> IO ()
part1 xs = print . magnitude $ L.foldl1 add xs

part2 xs = maximum [ magnitude (add x y) | (x,ys) <- select xs, y <- ys ]

select xs = [ (x, l++r) | (l,x:r) <- zip (L.inits xs) (L.tails xs) ]
