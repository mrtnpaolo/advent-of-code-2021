module Main (main) where

import Advent    (getInputLines,Coord(..),showCoords)
import Data.List qualified as L
import Data.Set  qualified as S

data Fold = FoldUp !Int | FoldLeft !Int deriving (Show)

main =
  do inp <- dotsAndFolds <$> getInputLines parse 13
     print (part1 inp)
     putStrLn (showCoords (part2 inp))
  where
    parse = words . map \case x | x `elem` ",=" -> ' '; x -> x
    readDot  [x,y]                  = C (read y) (read x)
    readFold ["fold","along","y",n] = FoldUp (read n)
    readFold ["fold","along","x",n] = FoldLeft (read n)
    dotsAndFolds xs = (S.fromList dots,folds)
      where
        (    map readDot  -> dots,
         []:(map readFold -> folds) ) = L.span (not . null) xs

fold (FoldUp   l) = S.map \case (C y x) | y > l -> C (l - (y - l)) x; c -> c
fold (FoldLeft l) = S.map \case (C y x) | x > l -> C y (l - (x - l)); c -> c

part1 (dots,f:_) = S.size (fold f dots)

part2 (dots,fs) = L.foldl (flip fold) dots fs
