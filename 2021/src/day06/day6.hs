import Data.List
import Data.List.Split (splitOn)
import qualified Data.MultiSet as MultiSet
import Debug.Trace (trace)

example = "3,4,3,1,2"

-- I tried this with normal arrays but that takes forever and ever. This finishes instantly
type Input = MultiSet.MultiSet Int

parse :: String -> Input
parse = MultiSet.fromList . map read . splitOn ","

evolve :: Int -> [Int]
evolve 0 = [6, 8]
evolve fish = [fish -1]

simulateDay :: Input -> Input
simulateDay = MultiSet.concatMap evolve

folding :: Input -> Int -> Input
folding a b | trace ("Simulating day " ++ show b ++ " Fishes: " ++ show (length a)) False = undefined
folding a b = simulateDay a

simulateDays :: Int -> Input -> Input
simulateDays days initial = foldl (folding) initial [1 .. days]

part1 input =
  let c = parse input
      end = simulateDays 80 c
   in length end

part2 input =
  let c = parse input
      end = simulateDays 256 c
   in length end

main = do
  content <- readFile "src/day06/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2