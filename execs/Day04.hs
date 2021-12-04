module Main (main) where

import Advent    (getInput)
import Data.List (unfoldr,partition,transpose)

main =
  do (balls,boards) <- getInput (parse . lines) 4
     let winners = play balls boards
     print (part1 winners)
     print (part2 winners)
  where
    parse (xs:_:ys) = (balls,boards)
      where
        balls = read @[Int] ('[' : xs ++ "]")

        boards = unfoldr collect ys
          where
            collect [] = Nothing
            collect xs = Just (board,drop 6 xs)
              where
                board = map (map (read @Int) . words) (take 5 xs)

play []     _      = []
play (n:ns) boards = map (score n) winners ++ play ns rest
  where
    (winners,rest) = partition winning (map (mark n) boards)

mark n = map (map replace)
  where
    replace x | x == n = -1 | otherwise = x

winning board = bingo board || bingo (transpose board)
  where
    bingo = any (all (-1 ==))

score ball b = ball * sum [ n | n <- concat b, n /= -1 ]

part1 = head

part2 = last
