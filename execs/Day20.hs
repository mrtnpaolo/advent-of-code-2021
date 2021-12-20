module Main (main) where

import Advent.Input    (getInput)
import Advent.List     (count)
import Advent.Coord    (Coord(..),bookreading,showCoords)
import Numeric         (readBin)
import Data.List       qualified as L
import Data.Set        qualified as S
import Data.IntSet     qualified as IS

main = enhance =<< getInput (parse . lines) 20

data Pic = Pic !Bool !(S.Set Coord)

size (Pic _ s) = length s

instance Show Pic where show (Pic _ s) = showCoords s

parse (a:[]:rows) = (s,pic)
  where
    s   = IS.fromList [ i | (i,'#') <- zip [0..] a ]
    pic = Pic False (S.fromList light)
    light = [ C y x | (y,xs) <- zip [0..] rows, (x,'#') <- zip [0..] xs ]

enhance (a,pic) =
  do let times n = iterate (tick a) pic !! n
         p2  = times 2
         p50 = times 50
     print (size p2)
     print (size p50)

tick a (Pic bg s) = Pic bg' s'
  where
    bg'  = IS.member (if bg then 511 else 0) a
    s'   = S.filter fg (S.fromList (bookreading `concatMap` S.toList s))

    fg c = IS.member n a /= bg'
      where
        n = fromDigits 2 [ if S.member d s /= bg then 1 else 0 | d <- bookreading c ]

fromDigits base = L.foldl' (\a i -> a*base + i) 0
