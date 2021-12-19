module Main (main) where

import Advent.Input       (getInput)
import Data.Char          (isDigit)
import Data.Maybe         (listToMaybe)
import Data.List          qualified as L
import Data.Set           qualified as S
import Data.Map.Strict    qualified as M
import Control.Monad      ((>=>))

main = solve =<< getInput parse 19

data C = C !Int !Int !Int deriving (Show,Eq,Ord)

data S = S !Int [C]

instance Show S where
  showsPrec _ (S n ps) =
    showString "--- scanner " . shows n . showString " ---\n" .
      L.foldl' showC id ps
    where
      showC f (C x y z) = f .
        shows x . showChar ',' .
        shows y . showChar ',' .
        shows z . showChar '\n'

parse = cut . map clean . lines
  where
    clean = map \case c | isDigit c || c `elem` "-" -> c; _ -> ' '
    cut [] = []
    cut ((words -> ["---",n,"---"]):xs) = S (read n) ps : cut xs'
      where (ps,xs') = points [] xs
    points ps ([])    = (ps,[])
    points ps ([]:xs) = (ps,xs)
    points ps ((words -> [x,y,z]):xs) = points (ps ++ [c]) xs
      where c = C (read x) (read y) (read z)

(C a0 a1 a2) <+> (C b0 b1 b2) = C (a0+b0) (a1+b1) (a2+b2)

(C a0 a1 a2) <-> (C b0 b1 b2) = C (a0-b0) (a1-b1) (a2-b2)

data M = M C C C deriving Show

m1 = -1

rs = filter ((1==) . det)
  [ M (C  1  0  0) (C  0  1  0) (C  0  0  1)
  , M (C  0  1  0) (C  1  0  0) (C  0  0  1)
  , M (C  1  0  0) (C  0  0  1) (C  0  1  0)
  , M (C  0  1  0) (C  0  0  1) (C  1  0  0)
  , M (C  0  0  1) (C  1  0  0) (C  0  1  0)
  , M (C  0  0  1) (C  0  1  0) (C  1  0  0)
  , M (C m1  0  0) (C  0  1  0) (C  0  0  1)
  , M (C  0 m1  0) (C  1  0  0) (C  0  0  1)
  , M (C m1  0  0) (C  0  0  1) (C  0  1  0)
  , M (C  0 m1  0) (C  0  0  1) (C  1  0  0)
  , M (C  0  0 m1) (C  1  0  0) (C  0  1  0)
  , M (C  0  0 m1) (C  0  1  0) (C  1  0  0)
  , M (C  1  0  0) (C  0 m1  0) (C  0  0  1)
  , M (C  0  1  0) (C m1  0  0) (C  0  0  1)
  , M (C  1  0  0) (C  0  0 m1) (C  0  1  0)
  , M (C  0  1  0) (C  0  0 m1) (C  1  0  0)
  , M (C  0  0  1) (C m1  0  0) (C  0  1  0)
  , M (C  0  0  1) (C  0 m1  0) (C  1  0  0)
  , M (C m1  0  0) (C  0 m1  0) (C  0  0  1)
  , M (C  0 m1  0) (C m1  0  0) (C  0  0  1)
  , M (C m1  0  0) (C  0  0 m1) (C  0  1  0)
  , M (C  0 m1  0) (C  0  0 m1) (C  1  0  0)
  , M (C  0  0 m1) (C m1  0  0) (C  0  1  0)
  , M (C  0  0 m1) (C  0 m1  0) (C  1  0  0)
  , M (C  1  0  0) (C  0  1  0) (C  0  0 m1)
  , M (C  0  1  0) (C  1  0  0) (C  0  0 m1)
  , M (C  1  0  0) (C  0  0  1) (C  0 m1  0)
  , M (C  0  1  0) (C  0  0  1) (C m1  0  0)
  , M (C  0  0  1) (C  1  0  0) (C  0 m1  0)
  , M (C  0  0  1) (C  0  1  0) (C m1  0  0)
  , M (C m1  0  0) (C  0  1  0) (C  0  0 m1)
  , M (C  0 m1  0) (C  1  0  0) (C  0  0 m1)
  , M (C m1  0  0) (C  0  0  1) (C  0 m1  0)
  , M (C  0 m1  0) (C  0  0  1) (C m1  0  0)
  , M (C  0  0 m1) (C  1  0  0) (C  0 m1  0)
  , M (C  0  0 m1) (C  0  1  0) (C m1  0  0)
  , M (C  1  0  0) (C  0 m1  0) (C  0  0 m1)
  , M (C  0  1  0) (C m1  0  0) (C  0  0 m1)
  , M (C  1  0  0) (C  0  0 m1) (C  0 m1  0)
  , M (C  0  1  0) (C  0  0 m1) (C m1  0  0)
  , M (C  0  0  1) (C m1  0  0) (C  0 m1  0)
  , M (C  0  0  1) (C  0 m1  0) (C m1  0  0)
  , M (C m1  0  0) (C  0 m1  0) (C  0  0 m1)
  , M (C  0 m1  0) (C m1  0  0) (C  0  0 m1)
  , M (C m1  0  0) (C  0  0 m1) (C  0 m1  0)
  , M (C  0 m1  0) (C  0  0 m1) (C m1  0  0)
  , M (C  0  0 m1) (C m1  0  0) (C  0 m1  0)
  , M (C  0  0 m1) (C  0 m1  0) (C m1  0  0)
  ]

det (M (C a0 a1 a2) (C b0 b1 b2) (C c0 c1 c2)) =
  a0*b1*c2 - a0*b2*c1 - a1*b0*c2 + a1*b2*c0 + a2*b0*c1 - a2*b1*c0

times :: M -> C -> C
(M (C a0 a1 a2) (C b0 b1 b2) (C c0 c1 c2)) `times` (C x y z) =
  C (x*a0 + y*a1 + z*a2) (x*b0 + y*b1 + z*b2) (x*c0 + y*c1 + z*c2)

alternatives ps = [ map (times m) ps | m <- rs ]

stitch _       known remain | M.null remain = known
stitch (i:ids) known remain                 = stitch knownids' known' remain'
  where
    knownids' = M.keys found ++ ids
    known'    = known  `M.union`      found
    remain'   = remain `M.difference` found

    (_,ps) = known M.! i
    found  = M.mapMaybe (match ps) remain

match :: S.Set C -> [C] -> Maybe (C,S.Set C)
match ps qs = listToMaybe
  [ (diff,qs2)
  | qs1 <- alternatives qs
  , (diff,qs2) <- [ (diff,S.fromList qs2)
                  | p <- S.toList ps
                  , q <- qs1
                  , let diff = p <-> q
                  , let qs2 = map (diff <+>) qs1 ]
  , S.size (S.intersection ps qs2) >= 12
  ]

solve ((S i ps):scanners) =
  do print $ S.size (S.unions beacons)
     print $ maximum [ manhattan p q | p <- centers, q <- centers ]
  where
    first = M.singleton i (C 0 0 0,S.fromList ps)
    rest  = M.fromList [ (i,ps) | (S i ps) <- scanners ]

    (centers,beacons) = unzip $ M.elems $ stitch [i] first rest

manhattan (C x0 y0 z0) (C x1 y1 z1) = abs (x0-x1) + abs (y0-y1) + abs (z0-z1)
