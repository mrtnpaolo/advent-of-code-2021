module Main (main) where

import Advent                   (getInputLines)
import Data.List qualified as L (nub,sort,foldl')

main =
  do inp <- getInputLines parse 22
     print (solve (focus 50 inp))
     print (solve inp)

parse = instr . words . map clean
  where
    clean c | c `elem` "=.," = ' ' | otherwise = c

data Cube = Cube !Span !Span !Span deriving Show
data Span = Span !Int !Int         deriving Show

instr
  [ switch
  , "x", read -> xm, read -> xM
  , "y", read -> ym, read -> yM
  , "z", read -> zm, read -> zM ]
  =
  ( switch == "on" ,
    Cube (Span xm (xM+1)) (Span ym (yM+1)) (Span zm (zM+1)) )

solve [] = 0
solve ((False,_):cs) = solve cs
solve ((True ,c):cs) = sum [ volume c | c <- pieces ] + solve cs
  where
    pieces = L.foldl' subdivide [c] [ c' | (_,c') <- cs ]
    subdivide cubes c' = concat [ subcubes c' c | c <- cubes ]

subcubes c1@(Cube c1sx c1sy c1sz) c2@(Cube c2sx c2sy c2sz)
  | not (overlap c1 c2) = [c2]
  | otherwise           = splits
  where
    splits =
      [ Cube sx sy sz
      | sx <- spans c1sx c2sx
      , sy <- spans c1sy c2sy
      , sz <- spans c1sz c2sz
      , let c = Cube sx sy sz
      , not (overlap c1 c)
      , overlap c2 c ]

spans (Span xm ym) (Span xM yM) =
  [ Span sx sy | sx <- coords | sy <- tail coords ]
  where
    coords = L.nub (L.sort [xm,ym,xM,yM])

overlap (Cube sx sy sz) (Cube sx' sy' sz') =
  intersect sx sx' && intersect sy sy' && intersect sz sz'

intersect (Span pm pM) (Span qm qM) = max pm qm < min pM qM

volume (Cube sx sy sz) = width sx * width sy * width sz

width (Span xm xM) = xM - xm

focus n cs =
  [ (b,Cube sx' sy' sz')
  | (b,Cube sx sy sz) <- cs
  , sx' <- clamp n sx
  , sy' <- clamp n sy
  , sz' <- clamp n sz ]

clamp n (Span xm xM)
  | xm' < xM' = [Span xm' xM']
  | otherwise = []
  where
    xm' = max (-n)  xm
    xM' = min (n+1) xM
