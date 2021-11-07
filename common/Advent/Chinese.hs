module Advent.Chinese
  ( chinese, chineseSafe
  ) where

import Data.Ord (comparing)
import Data.List (tails,sortBy)

-- | unsafe version: doesn't check coprimality of moduli, nor a_i < n_i
chinese :: Integral a => [(a,a)] -> (a,a)
chinese congruences = go first rest
  where
    -- speed up the search by using the biggest moduli first
    (first:rest) = sortBy (comparing (negate . snd)) congruences

    -- sieve
    go (a,n) ((b,m):rest)
      | null rest = (x,n*m)
      | otherwise = go (x,n*m) rest
      where
        x = head [ x | k <- [0..]
                     , let x = a + k * n
                     , x `mod` m == b ]

-- | safe version: checks for coprimality of moduli as well as a_i < n_i
chineseSafe :: Integral a => [(a,a)] -> Maybe (a,a)
chineseSafe congruences
  | pairWiseCoprimeModuli = Just (go first rest)
  | otherwise             = Nothing

  where
    pairWiseCoprimeModuli =
      and [ n `mod` m /= 0 | (n:ms) <- tails moduli, m <- ms ]

    moduli = map snd normalized

    normalized = [ (a `mod` n,n) | (a,n) <- sortBy (comparing (negate . snd)) congruences ]

    (first:rest) = normalized

    go (a,n) ((b,m):rest)
      | null rest = (x,n*m)
      | otherwise = go (x,n*m) rest
      where
        x = head [ x | k <- [0..]
                     , let x = a + k * n
                     , x `mod` m == b ]
