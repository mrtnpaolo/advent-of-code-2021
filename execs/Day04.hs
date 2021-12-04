module Main (main) where

import Advent (getInput)

import Data.Ix   (range)
import Data.List (inits,unfoldr,partition)

import Data.IntSet        qualified as IS (fromList,member)
import Data.Array.Unboxed qualified as A  (UArray,listArray,elems,(!))

type Board = A.UArray (Int,Int) Int

main =
  do (turns,boards) <- getInput (parse . lines) 4
     let winners = play turns boards
     print (part1 winners)
     print (part2 winners)
  where
    parse (xs:_:ys) = (turns,boards)
      where
        turns = inits (read @[Int] ('[' : xs ++ "]"))

        boards = unfoldr collect ys

        collect [] = Nothing
        collect xs = Just (board,drop 6 xs)
          where
            board = A.listArray ((0,0),(4,4)) entries :: Board
            entries = concatMap (map (read @Int) . words) (take 5 xs)

play [] _ = []
play (draws:nexts) boards = map (score draws) winners ++ play nexts rest
  where
    (winners,rest) = partition (winning draws) boards

winning :: [Int] -> Board -> Bool
winning (IS.fromList -> drawn) board =
  or [ all (`IS.member` drawn) [ board A.! c | c <- cs ]
     | cs <- rows ++ cols ]
  where
    rows = [ [ (i,j) | j <- [0..4] ] | i <- [0..4] ]
    cols = [ [ (i,j) | i <- [0..4] ] | j <- [0..4] ]

score draws b = last draws * sum [ n | n <- A.elems b, n `notElem` draws ]

part1 = head

part2 = last
