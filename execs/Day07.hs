module Main (main) where

import Advent

main =
  do crabs <- getInput parse 7
     print (part1 crabs)
     print (part2 crabs)
  where
    parse xs = read @[Int] $ '[':xs++"]"

optimal cost xs = minimum (map cost [minimum xs..maximum xs])

part1 xs = optimal (\x -> sum [           abs (n-x)  | n <- xs]) xs

part2 xs = optimal (\x -> sum [ triangle (abs (n-x)) | n <- xs]) xs
  where
    triangle n = n*(n+1)`div`2
