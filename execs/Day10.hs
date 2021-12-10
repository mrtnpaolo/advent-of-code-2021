module Main (main) where

import Advent      (getInputLines)
import Data.List   (foldl',sort)
import Data.Either (partitionEithers)

main =
  do (corrupted,incomplete) <- partitionEithers <$> getInputLines parse 10
     print (part1 corrupted)
     print (part2 incomplete)

parse = go []
  where
    go seen             []      = Right seen     -- string is incomplete
    go seen@(~(s:rest)) (x:xs)
      | open x                  = go (x:seen) xs
      | close x && s == match x = go    rest  xs
      |            s /= match x = Left x         -- string is corrupted

open  x = x `elem` "([{<"

close x = x `elem` ")]}>"

match ')' = '('
match ']' = '['
match '}' = '{'
match '>' = '<'

match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

part1 = sum . map score
  where
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137

part2 = middle . map (base5 . map score . map match)
  where
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4

    base5 = foldl' (\a i -> a*5 + i) 0
    middle xs = sort xs !! (length xs `div` 2)
