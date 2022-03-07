import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Sequence (fromList, mapWithIndex)

example = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

type Position = (Int, Int)

type HeightMap = M.Map Int Int

keyFromPosition :: Position -> Int
keyFromPosition (x, y) = x + y * 1000

parseLine :: Int -> String -> [(Int, Int)]
parseLine lineNumber = toList . mapWithIndex (\x a -> let key = keyFromPosition (x, lineNumber) in (key, digitToInt a)) . fromList

parse :: String -> HeightMap
parse = M.fromList . concat . toList . mapWithIndex parseLine . fromList . lines

findWithKey :: Ord a => M.Map a b -> a -> Maybe (a, b)
findWithKey map key =
  let value = map M.!? key
   in fmap (\v -> (key, v)) value

getAdjacent :: HeightMap -> Int -> [(Int, Int)]
getAdjacent heightMap key =
  catMaybes
    [ heightMap `findWithKey` (key + 1),
      heightMap `findWithKey` (key + 1000),
      heightMap `findWithKey` (key - 1),
      heightMap `findWithKey` (key - 1000)
    ]

isLowPoint :: HeightMap -> Int -> Int -> Bool
isLowPoint heightMap key height = all ((> height) . snd) (getAdjacent heightMap key)

part1 input =
  let heightMap = parse input
      lowPoints = M.filterWithKey (isLowPoint heightMap) heightMap
   in (sum . map ((+ 1) . snd) . M.toList) lowPoints

findBasin :: HeightMap -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
findBasin heightMap basin (key, value)
  | value == 9 = basin
  | (key, value) `elem` basin = basin
  | otherwise = foldl (findBasin heightMap) ((key, value) : basin) (getAdjacent heightMap key)

basinSize :: [(Int, Int)] -> Int
basinSize = length

part2 input =
  let heightMap = parse input
      lowPoints = (M.toList . M.filterWithKey (isLowPoint heightMap)) heightMap
      basins = map (findBasin heightMap []) lowPoints
   in (product . take 3 . reverse . sort . map basinSize) basins

main = do
  content <- readFile "src/day09/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2