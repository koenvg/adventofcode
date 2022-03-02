import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map

example = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"

type Coordinate = (Int, Int)

type VentMap = Map.Map String Int

key :: Coordinate -> String
key = show

parseLine :: String -> (Coordinate, Coordinate)
parseLine line =
  let [from, _, to] = words line
      [x1, y1] = (map read . splitOn ",") from
      [x2, y2] = (map read . splitOn ",") to
   in ((x1, y1), (x2, y2))

parseInput :: String -> [(Coordinate, Coordinate)]
parseInput = map parseLine . lines

isStraightLine :: (Coordinate, Coordinate) -> Bool
isStraightLine ((x1, y1), (x2, y2)) = x1 == x2 || y2 == y1

markVent :: Maybe Int -> Maybe Int
markVent (Just v) = Just (v + 1)
markVent Nothing = Just 1

markVentOnMap :: Coordinate -> VentMap -> VentMap
markVentOnMap c = Map.alter markVent (key c)

-- walkAndMark :: Coordinate -> Coordinate -> VentMap -> VentMap
walkAndMark (from, to) map =
  let points = extractPoints (from, to)
      markedMap = foldr markVentOnMap map points
   in markedMap

extractPoints :: (Coordinate, Coordinate) -> [Coordinate]
extractPoints ((x1, y1), (x2, y2))
  | x1 == x2 = [(x1, y) | y <- y1 ... y2]
  | y1 == y2 = [(x, y1) | x <- x1 ... x2]
  | otherwise = zip (x1 ... x2) (y1 ... y2)

-- Sweet as fuck, stolen from the person in the readme.
(...) :: Int -> Int -> [Int]
x ... y
  | x <= y = [x .. y]
  | otherwise = reverse [y .. x]

countOverlap :: VentMap -> Int
countOverlap = Map.foldl (\acc v -> if v > 1 then acc + 1 else acc) 0

part1 input =
  let parsed = (filter isStraightLine . parseInput) input
      markedMap = foldr walkAndMark Map.empty parsed
      overlap = countOverlap markedMap
   in overlap

part2 input =
  let parsed = parseInput input
      markedMap = foldr walkAndMark Map.empty parsed
      overlap = countOverlap markedMap
   in overlap

main = do
  content <- readFile "src/day05/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2
