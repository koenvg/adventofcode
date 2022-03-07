import qualified Control.Arrow as Data.Bifunctor
import Data.IntMultiSet (distinctElems)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M

example = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

type Input = ([String], [String])

parse :: String -> [Input]
parse = map parseLine . lines

parseLine :: String -> Input
parseLine input =
  let [a, b] = splitOn " | " input
      signals = splitOn " " a
      output = splitOn " " b
   in (signals, output)

-- This means is it a 1,4,7 or 8 which have a unique length
isDigitSimple :: String -> Bool
isDigitSimple digit =
  case length digit of
    4 -> True
    2 -> True
    3 -> True
    7 -> True
    _ -> False

identify :: [String] -> M.Map String Char
identify l = M.fromList $ zip (map sort [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]) ['0' ..]
  where
    d1 = f (\x -> length x == 2)
    d4 = f (\x -> length x == 4)
    d8 = f (\x -> length x == 7)
    d7 = f (\x -> length x == 3)
    d2 = f (\x -> length x == 5 && overlaps x d4 2)
    d3 = f (\x -> length x == 5 && overlaps x d4 3 && contains x d1)
    d5 = f (\x -> length x == 5 && overlaps x d4 3 && not (contains x d1))
    d6 = f (\x -> length x == 6 && not (contains x d1))
    d9 = f (\x -> length x == 6 && contains x d3)
    d0 = f (\x -> length x == 6 && not (contains x d3) && contains x d1)
    f p = case find p l of
      Just x -> x
      Nothing -> error "logic error"
    contains a = all (`elem` a)
    overlaps a b n = let bina = filter (`elem` a) b in length bina == n

part1 input =
  let lines = parse input
      numberOf147or8 = map (\(_, output) -> (length . filter isDigitSimple) output) lines
   in sum numberOf147or8

findSolution :: (M.Map String Char, [String]) -> String
findSolution (board, output) = map (\number -> board M.! sort number) output

part2 input =
  let lines = parse input
      identified = map (Data.Bifunctor.first identify) lines
      solutions = map (read . findSolution) identified
   in sum solutions

main = do
  content <- readFile "src/day08/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2

-- T = in 7 but not in 1
-- Middle = in 4 but not in 0 (6 highlighted and will be present in 9 and 6)
-- TL = once we know middle tl is number that is left in 4 but not in 1
-- BL = Find the 9 (filter out the 0 we can find that because we have the middle now) -> in 8 but not in 9
-- TR = find the 6 (Filter out 0 and 9) -> in 8 but not in 6
-- BR = other one in 1
-- b = remainder