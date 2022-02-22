import Control.Monad ()
import Data.Char (chr)
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : rest) = zip (x : rest) rest

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [a] = []
triples (x : y : rest) = zip3 (x : y : rest) (y : rest) rest

part1 :: (Num a, Ord a) => [a] -> Int
part1 list = foldl (\count (a, b) -> if a < b then count + 1 else count) 0 (pairs list)

part2 :: (Num a, Ord a) => [a] -> Int
part2 list = part1 slidingWindow
  where
    slidingWindow = map (\(a, b, c) -> a + b + c) $ triples list

main = do
  let list = []
  handle <- openFile "src/day1/input.txt" ReadMode
  contents <- hGetContents handle
  let l = map read $ lines contents
      answer = part1 l
      answer2 = part2 l
  print $ "Answer 1: " ++ [chr answer]
  print $ "Answer 2: " ++ [chr answer2]
  hClose handle