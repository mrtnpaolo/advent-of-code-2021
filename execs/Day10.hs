module Main (main) where

import Advent
import Numeric
import Data.Ix
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Either
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Data.IntSet        qualified as IS
import Data.IntMap.Strict qualified as IM
import Data.Array.Unboxed qualified as A
import Debug.Trace

main =
  do inp <- getInputLines parse 10
     print (part1 inp)
     print (part2 inp)
  where
    parse = id

part1 = sum . map score . lefts . map corruptedChar

corruptedChar xs = go [] xs
  where
    go seen [] = Right seen -- allow incomplete
    go seen (x:xs)
      | open x  = go (inc x seen) xs
      | close x
      , counterpart /= head seen = Left x
      | counterpart == head seen = go (dec counterpart seen) xs
        where
          counterpart = matching x
          inc x seen = (x:seen)
          dec x (_:seen) = seen

matching ')' = '('
matching ']' = '['
matching '}' = '{'
matching '>' = '<'
matching '(' = ')'
matching '[' = ']'
matching '{' = '}'
matching '<' = '>'

open  x = x `elem` "([{<"
close x = x `elem` ")]}>"

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

part2 ls = middle $ map autocompleteScore $ map (map matching) incompletes
  where
    incompletes = rights (map corruptedChar ls)

score2 ')' = 1
score2 ']' = 2
score2 '}' = 3
score2 '>' = 4

autocompleteScore = L.foldl' f 0
  where
    f acc x = acc*5 + score2 x

middle xs = L.sort xs !! (length xs `div` 2)
