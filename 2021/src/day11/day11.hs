{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (Foldable (toList))
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Sequence (fromArray, fromList, mapWithIndex)
import GHC.Real ((%))
import Text.Printf (printf)

example = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

lookupKey :: Eq v => v -> M.Map k v -> Maybe k
lookupKey val = M.foldrWithKey go Nothing
  where
    go key value found =
      if value == val
        then Just key
        else found

type Grid = M.Map Int Int

parseLine :: Int -> String -> [(Int, Int)]
parseLine y = toList . mapWithIndex (\x c -> (y * 10 + x, digitToInt c)) . fromList

parse :: String -> Grid
parse = M.fromList . concat . mapWithIndex parseLine . fromList . lines

raiseEnergyByFlash :: Int -> Int
raiseEnergyByFlash 0 = 0
raiseEnergyByFlash 10 = 10
raiseEnergyByFlash v = v + 1

step :: Grid -> Grid
step = M.map (+ 1)

adjacentKeys :: Int -> [Int]
adjacentKeys k
  | k `mod` 10 == 0 = [k + 10, k -10, k + 1, k + 11, k -9]
  | k `mod` 10 == 9 = [k - 1, k + 10, k - 10, k + 9, k - 11]
  | otherwise = [k + 1, k - 1, k + 10, k - 10, k + 9, k -9, k + 11, k -11]

flashOctopus :: Int -> Grid -> Grid
flashOctopus at grid =
  let gridAfterInitialFlash = M.update (\a -> Just 0) at grid
      gridAfterFlash = foldr (M.update (Just . raiseEnergyByFlash)) gridAfterInitialFlash (adjacentKeys at)
   in gridAfterFlash

flash :: Grid -> Grid
flash g =
  let willFlash = lookupKey 10 g
      jos = case willFlash of
        Nothing -> g
        Just k -> flash (flashOctopus k g)
   in jos

evolve :: Int -> Grid -> [Grid]
evolve steps grid = foldl folding [grid] [1 .. steps]
  where
    folding :: [Grid] -> Int -> [Grid]
    folding (x : xs) _ = (flash . step) x : x : xs

countFlashes :: Grid -> Int
countFlashes = M.size . M.filter (== 0)

showGrid :: Grid -> String
showGrid = M.foldlWithKey (\s k v -> if k `mod` 10 == 0 then s ++ "\n" ++ [intToDigit v] else s ++ [intToDigit v]) "\n\n"

part1 input =
  let grid = parse input
      after = evolve 100 grid
      totalFlashes = (sum . map countFlashes) after
   in totalFlashes

isSynchronizing :: Grid -> Bool
isSynchronizing = all (== 0)

part2 input =
  let grid = parse input
      after = evolve 700 grid
      first = findIndex isSynchronizing (reverse after)
   in first

main = do
  content <- readFile "./src/day11/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2