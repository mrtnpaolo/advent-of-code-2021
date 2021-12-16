module Main (main) where

import Advent      (getInput)
import Numeric     (readBin,showBin,readHex)
import Text.Printf (printf)

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

fromBin bytes = n where [(n,"")] = readBin bytes

parse hex = let (pkt,_) = packet bits in pkt
  where
    bits = concatMap fromHex hex

packet bits =
  case typ of
    4 -> lit ver (drop 6 bits)
    _ -> app ver typ (drop 6 bits)

  where
    ver = fromBin (take 3 bits)
    typ = fromBin (take 3 (drop 3 bits))

lit ver bits = (Lit ver value,rest)
  where
    value = fromBin bytes

    (bytes,rest) = go [] bits

    go seen xs
      | (x:new) <- take 5 xs =
        case x of
          '1' -> go (seen ++ new) (drop 5 xs)
          '0' | all ('0'==) (drop 5 xs) -> (seen ++ new,[]       )
              | otherwise               -> (seen ++ new,drop 5 xs)
      | otherwise = (seen,xs)

app ver typ ('0':bits) = (App ver typ pkts,rest)
  where
    pkts = go (take n xs)
    rest = drop n xs

    n = fromBin (take 15 bits)
    xs = drop 15 bits

    go []   = []
    go bits = let (pkt,rest) = packet bits in pkt : go rest

app ver typ ('1':bits) = (App ver typ pkts,rest)
  where
    (pkts,rest) = go n id xs

    n = fromBin (take 11 bits)
    xs = drop 11 bits

    go 0 ps rest = (ps [],rest)
    go n ps bits = go (n-1) (ps . (p :)) rest where (p,rest) = packet bits

part1 = sum . versions . parse

versions (Lit ver _)      = [ver]
versions (App ver _ pkts) = ver : concatMap versions pkts

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
