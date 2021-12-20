module Main (main) where

import Advent.Input    (getInput)
import Advent.List     (count)
import Advent.Coord    (Coord(..),bookreading,withCoords,boundingBox,addCoord)
import Numeric         (readBin)
import Data.Ix         (range)
import Data.Char       (intToDigit)
import Data.Map.Strict qualified as M
import Control.Applicative ((<|>))

main =
  do solve =<< getInput parse 20

parse (lines -> (a:[]:img)) = (map toBin a,M.fromList $ withCoords toBin img)
  where
    toBin '#' = 1; toBin '.' = 0

solve (a,m) =
  do print (M.size $ M.filter (1 ==) m2)
     print (M.size $ M.filter (1 ==) m50)
  where
    (m2 ,_) = iterate (step a) (m,False) !! 2
    (m50,_) = iterate (step a) (m,False) !! 50

step a (m,flag) = (m',not flag)
  where
    Just (cm,cM) = boundingBox (M.keys m)
    margin = 3
    box    = ( cm `addCoord` (C (-margin) (-margin)) ,
               cM `addCoord` (C margin margin)       )
    m'     = M.fromList [ (c,enhance flag a m c) | c <- range box ]

enhance flag a m c = enhanced
  where
    cs = bookreading c
    void | a !! 0 == 0 = 0
         | otherwise   = fromEnum flag
    digits = [ intToDigit v | c' <- cs, Just v <- [m M.!? c' <|> Just void] ]
    [(n,_)] = readBin digits
    enhanced = a !! n
