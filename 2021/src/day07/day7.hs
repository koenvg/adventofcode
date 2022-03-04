import Data.List (elemIndex, sort)
import Data.List.Split (splitOn)

example = "16,1,2,0,4,2,7,1,2,14"

parse :: String -> [Int]
parse = sort . map read . splitOn ","

totalFuelCost :: [Int] -> Int -> Int
totalFuelCost crabPositions position = sum $ map (\a -> abs $ a - position) crabPositions

totalFuelCost2 :: [Int] -> Int -> Int
totalFuelCost2 crabPositions position = sum $ map (\a -> sum [1 .. (abs $ a - position)]) crabPositions

median :: [Int] -> Int
median list =
  let middle = fromIntegral (length list) / 2
   in list !! ceiling middle

mean :: [Int] -> Double
mean list = a / b
  where
    a = fromIntegral $ sum list
    b = fromIntegral $length list

part1 input =
  let crabPositions = parse input
      bestPosition = median crabPositions
      total = totalFuelCost crabPositions bestPosition
   in total

part2 input =
  let crabPositions = parse input
      best = [floor $ mean crabPositions, ceiling $ mean crabPositions]
      total = (minimum . map (totalFuelCost2 crabPositions)) best
   in total

main = do
  content <- readFile "src/day07/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2
