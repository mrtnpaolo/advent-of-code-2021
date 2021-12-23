module Main (main) where

import Advent
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
import Data.Array.Unboxed ((//))
import Debug.Trace

type Cave = A.UArray Coord Char

main =
  do inp <- getInput parse 23
     putStrLn (p inp)
     mapM_ print (part1 inp)
     print (part2 inp)

parse = distinguish . toArray . withCoords fill . lines
  where
    fill ' ' = '#'
    fill  c  =  c
    toArray ps = A.array bounds ps :: Cave
      where
        bounds = (minimum cs,maximum cs)
        cs = [ c | (c,_) <- ps ]
    distinguish a = a' :: Cave
      where
        a' = a // ambers // bronzes // coppers // deserts
        ambers  = zip [ c | (c,'A')<-A.assocs a] "aAxX"
        bronzes = zip [ c | (c,'B')<-A.assocs a] "bByY"
        coppers = zip [ c | (c,'C')<-A.assocs a] "cCzZ"
        deserts = zip [ c | (c,'D')<-A.assocs a] "dDwW"
{-
        ambers  = let [c1,c2] = [c| (c,'A')<-A.assocs a ] in [(c1,'a'),(c2,'A')]
        bronzes = let [c1,c2] = [c| (c,'B')<-A.assocs a ] in [(c1,'b'),(c2,'B')]
        coppers = let [c1,c2] = [c| (c,'C')<-A.assocs a ] in [(c1,'c'),(c2,'C')]
        deserts = let [c1,c2] = [c| (c,'D')<-A.assocs a ] in [(c1,'d'),(c2,'D')]
-}

p :: Cave -> String
p a = drawCoords (M.fromList (A.assocs a))

done :: Cave -> Bool
done a =
  and [ destX 'A' == destX (toUpper $ a A.! C y 3) | y <- [2,3,4,5] ] &&
  and [ destX 'B' == destX (toUpper $ a A.! C y 5) | y <- [2,3,4,5] ] &&
  and [ destX 'C' == destX (toUpper $ a A.! C y 7) | y <- [2,3,4,5] ] &&
  and [ destX 'D' == destX (toUpper $ a A.! C y 9) | y <- [2,3,4,5] ]

{-
  all ('B' ==) [ toUpper $ a A.! C y x | y <- [2,3], x <- [5] ] &&
  all ('C' ==) [ toUpper $ a A.! C y x | y <- [2,3], x <- [7] ] &&
  all ('D' ==) [ toUpper $ a A.! C y x | y <- [2,3], x <- [9] ]
-}

type A = (Coord,Char)
type Energy = Int

data T = T !Cave !(S.Set A) !(S.Set A)

en (toUpper -> 'A') = 1
en (toUpper -> 'X') = 1

en (toUpper -> 'B') = 10
en (toUpper -> 'Y') = 10

en (toUpper -> 'C') = 100
en (toUpper -> 'Z') = 100

en (toUpper -> 'D') = 1000
en (toUpper -> 'W') = 1000

instance Show T where
  show (T a burrowed hallway {- e -}) =
    unlines $ lines (p a) ++
      [ unwords ["burrowed:", [ x | (_,x) <- S.toList burrowed] ]
      , unwords ["hallway:" , [ x | (_,x) <- S.toList hallway ] ]
      --, unwords ["energy:", show e ]
      ]

-- bfsOn   :: Ord r => (a -> r) -> (a -> [a]          ) -> [a] -> [a]
-- astarOn :: Ord r => (a -> r) -> (a -> [(a,Int,Int)]) ->  a  -> [(a,Int)]

solve a = best
  where
    best = L.find (\(T cave _ _,e) -> done cave) search

    search = astarOn repr nexts begin

    repr (T cave _ _) = cave

    begin = T a burrowed hallway
      where
        burrowed = S.fromList $ filter (not . done) $
          [ (c,x) | (c,x) <- A.assocs a, isAlpha x ]
          where
            --done (C 3 x,name) = x == destX name
            done (C y x,name) = False

        hallway  = S.fromList []

    nexts :: T -> [(T,Int,Int)]

    nexts (T cave burrowed hallway)
      | done cave = []
      | otherwise = ns
      where
        ns = entering ++ exiting

        exiting = [ (t',e',0)
                  | (c ,x ) <- S.toList burrowed
                  , (c',e') <- reachableHallway cave x c
                  , let t' = toT x c c'
                  ]
          where
            toT x c c' = T cave' burrowed' hallway'
              where
                cave'     = cave // [(c',x),(c,'.')]
                burrowed' = S.delete (c ,x) burrowed
                hallway'  = S.insert (c',x) hallway

        entering = [ (t',e',0)
                   | (c ,x) <- S.toList hallway
                   , (c',e') <- reachableDestination cave x c
                   , let t' = toT x c c' ]
          where
            toT x c c' = T cave' burrowed hallway'
              where
                cave'     = cave // [(c',x),(c,'.')]
                hallway'  = S.delete (c,x) hallway


{-
solve a = take 4 $ L.sortOn tenergy bests
  where
    tenergy (T _ _ _ e) = e

    bests = L.filter (\(T cave _ _ _) -> done cave) search

    search = bfsOn repr nexts [begin]

    repr (T cave _ _ e) = (cave,e)

    begin = T a burrowed hallway 0

    burrowed = S.fromList $ filter (not . done) [ (c,x) | (c,x) <- A.assocs a, isAlpha x ]
      where
        done (C 3 x,name) = x == destX name
        done (C y x,name) = False

    hallway  = S.fromList []

    nexts :: T -> [T]

    nexts (T cave burrowed hallway e)
      | done cave = []
      | otherwise = ns
      where
        ns = entering ++ exiting

        exiting = [ toT x c c' e
                  | (c ,x) <- S.toList burrowed
                  , (c',e) <- reachableHallway cave x c ]
          where
            toT x c c' e' = T cave' burrowed' hallway' (e+e')
              where
                cave'     = cave // [(c',x),(c,'.')]
                burrowed' = S.delete (c ,x) burrowed
                hallway'  = S.insert (c',x) hallway

        entering = [ toT x c c' e
                   | (c ,x) <- S.toList hallway
                   , (c',e) <- reachableDestination cave x c ]
          where
            toT x c c' e' = T cave' burrowed hallway' (e+e')
              where
                cave'     = cave // [(c',x),(c,'.')]
                hallway'  = S.delete (c,x) hallway

-- bfs   :: Ord a => (a -> [a]) -> a -> [a]
-- bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]

-}

reachableHallway :: Cave -> Char -> Coord -> [(Coord,Energy)]
reachableHallway cave x from = search
  where
    search =
      [ (to,cost)
      | (to,cost) <- bfsOn repr nexts [(from,0)]
      , not (aboveBurrow to) ]
    repr = fst
    nexts (c,e) =
      [ (c',e + en x)
      | c' <- cardinal c
      , '.' == cave A.! c' ]

aboveBurrow (C _ x) = x `elem` [3,5,7,9]

destX (toUpper -> 'A') = 3
destX (toUpper -> 'X') = 3

destX (toUpper -> 'B') = 5
destX (toUpper -> 'Y') = 5

destX (toUpper -> 'C') = 7
destX (toUpper -> 'Z') = 7

destX (toUpper -> 'D') = 9
destX (toUpper -> 'W') = 9

destX _ = -1

reachableDestination :: Cave -> Char -> Coord -> [(Coord,Energy)]
reachableDestination cave name from = dests
  where
    dests =
      case search of
        [d]          -> [d]
        ds@[_,_]     -> [maximum ds]
        ds@[_,_,_]   -> [maximum ds]
        ds@[_,_,_,_] -> [maximum ds]
        _            -> []
    search =
      [ (to,cost)
      | (to@(C y x),cost) <- bfsOn repr nexts [(from,0)]
      , x == destX name
      , y > 1 ]
    repr = fst
    nexts (c,e) =
      [ (c',e + en name)
      | c' <- cardinal c
      , '.' == cave A.! c' ]

t =
  do cave <- getInput parse 23
     print (reachableHallway cave 'b' (C 2 3))

part1 a = solve a

part2 = const ()
