module Main (main) where

import Advent      (getInput)
import Numeric     (readBin,showBin,readHex)
import Text.Printf (printf)
import Data.List   qualified as L

main =
  do inp <- getInput id 16
     print (part1 inp)
     print (part2 inp)

type Version = Int
type Value   = Int
type TypeID  = Int

data Packet
  = Lit Version Value
  | App Version TypeID [Packet]
    deriving Show

fromHex byte = printf "%04b" (n :: Int) where [(n,"")] = readHex [byte]

fromBin bits = n where [(n,"")] = readBin bits

parse hex = head (L.unfoldr go (concatMap fromHex hex))
  where
    go []   = Nothing
    go bits = Just (packet bits)

packet bits =
  case typ of
    4 -> lit ver (drop 6 bits)
    _ -> app ver typ (drop 6 bits)

  where
    ver = fromBin (take 3 bits)
    typ = fromBin (take 3 (drop 3 bits))

lit ver bits = (Lit ver value,rest)
  where
    value = fromBin valueBits

    (valueBits,rest) = go [] bits

    go acc []         = (acc,[])
    go acc ('0':bits) = (acc ++ take 4 bits,drop 4 bits)
    go acc ('1':bits) = go (acc ++ take 4 bits) (drop 4 bits)

app ver typ ('0':bits) = (App ver typ ps,rest)
  where
    ps = go (take n xs)
    rest = drop n xs

    n = fromBin (take 15 bits)
    xs = drop 15 bits

    go []   = []
    go bits = let (p,rest) = packet bits in p : go rest

app ver typ ('1':bits) = (App ver typ ps,rest)
  where
    (ps,rest) = go n id xs

    n = fromBin (take 11 bits)
    xs = drop 11 bits

    go 0 ps rest = (ps [],rest)
    go n ps bits = go (n-1) (ps . (p :)) rest where (p,rest) = packet bits

part1 = sum . versions . parse

versions (Lit ver _)    = [ver]
versions (App ver _ ps) = ver : concatMap versions ps

part2 = eval . parse

eval (Lit _ val) = val

eval (App _ 0 ps) = sum     (map eval ps)
eval (App _ 1 ps) = product (map eval ps)
eval (App _ 2 ps) = minimum (map eval ps)
eval (App _ 3 ps) = maximum (map eval ps)

eval (App _ 5 [a,b]) = if eval a >  eval b then 1 else 0
eval (App _ 6 [a,b]) = if eval a <  eval b then 1 else 0
eval (App _ 7 [a,b]) = if eval a == eval b then 1 else 0

t = mapM_ print (zip t2 t1)
t1 = map parse ts
t2 = map eval t1
ts =
  [ "D2FE28"
  , "38006F45291200"
  , "EE00D40C823060"
  , "8A004A801A8002F478"
  , "620080001611562C8802118E34"

  , "C200B40A82"
  , "04005AC33890"
  , "880086C3E88112"
  , "CE00C43D881120"
  , "D8005AC2A8F0"
  , "F600BC2D8F"
  , "9C005AC2F8F0"
  , "9C0141080250320F1802104A08"
  ]
