module Main (main) where

import Advent          (getInputLines,count,dfs)
import Data.Char       (isLower,isUpper)
import Data.Set        qualified as S
import Data.Map.Strict qualified as M

main =
  do links <- toLinks <$> getInputLines parse 12
     mapM_ print (M.toList links)
     let paths = part1 links
     --mapM_ print paths
     print (length paths)
     let paths2 = part2 links
     --mapM_ print paths2
     print (length paths2)
  where
    parse = words . map \case '-' -> ' '; x -> x
    toLinks inp = M.fromListWith (++) (fixends (asgiven ++ opposite))
      where
        asgiven  = map (\[a,b] -> (a,[b])) inp
        opposite = map (\[a,b] -> (b,[a])) inp
        fixends  = map \case ("end",_) -> ("end",[]); x -> x
                 . map \(from,to) -> (from,filter ("start" /=) to)

type Path = [String]

type Links = M.Map String [String]

type Picker = Maybe String
           -> Path
           -> String
           -> [(Path, Maybe String)]

explore :: Picker -> Links -> [Path]
explore p links = paths
  where
    paths = filter complete search

    complete ("end":_) = True
    complete _         = False

    search = map fst (dfs next (["start"],Nothing))

    next (path@(here:_),dup) = concatMap allowed (links M.! here)
      where
        allowed there@(t:_)
          | isUpper t = [ (there:path,dup) ]
          | otherwise = p dup path there

part1 = explore pick

pick dup path there
  | there `elem` path = []
  | otherwise         = [ (there:path,dup) ]

part2 = explore pick'

pick' Nothing  = pick2
pick' dup      = pick dup

pick2 path there = [ (there:path,dup) ]
  where
    dup | there `elem` path = Just there | otherwise = Nothing
