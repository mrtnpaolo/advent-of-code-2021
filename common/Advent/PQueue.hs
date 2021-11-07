module Advent.PQueue
  ( PQueue(Empty,(:<|))
  , insert, singleton, fromList
  , Advent.PQueue.null, view, viewWithPriority
  ) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM

data PQueue a = PQ (IntMap [a])

pattern Empty :: PQueue a
pattern Empty <- (Advent.PQueue.null -> True)
  where
    Empty = PQ IM.empty

pattern (:<|) :: a -> PQueue a -> PQueue a
pattern v :<| q <- (view -> Just (v,q))

insert :: Int -> a -> PQueue a -> PQueue a
insert k v (PQ m) = PQ (IM.alter f k m)
  where
    f Nothing   = Just [v]
    f (Just vs) = Just (v:vs)

singleton :: Int -> a -> PQueue a
singleton p v = PQ (IM.singleton p [v])

fromList :: [(Int,a)] -> PQueue a
fromList xs = PQ $
  IM.fromListWith (++) [ (p,[v]) | (p,v) <- xs ]

null :: PQueue a -> Bool
null (PQ m) = IM.null m

view :: PQueue a -> Maybe (a,PQueue a)
view (PQ m) =
  do ((p,vs),m') <- IM.minViewWithKey m
     case vs of
      [] -> error "malformed queue"
      [v] -> Just (v,PQ m')
      (v:vs) -> p' `seq` Just (v,p')
        where
          p' = PQ (IM.insert p vs m')

viewWithPriority :: PQueue a -> Maybe (Int,a,PQueue a)
viewWithPriority (PQ m) =
  do ((p,vs),m') <- IM.minViewWithKey m
     case vs of
      [] -> error "malformed queue"
      [v] -> Just (p,v,PQ m')
      (v:vs) -> p' `seq` Just (p,v,p')
        where
          p' = PQ (IM.insert p vs m')

instance Show a => Show (PQueue a) where
  showsPrec prec (PQ m) =
    showParen (prec >= 11) $
    showString "fromList " .
    shows [ (p,v) | (p,vs) <- IM.toList m, v <- vs ]

instance Read a => Read (PQueue a) where
  readsPrec prec =
    readParen (prec >= 11) $ \s ->
      do ("fromList",s) <- lex s
         (xs,s)         <- reads s
         pure (fromList xs,s)
