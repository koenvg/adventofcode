import Control.Monad ()
import Data.Bits
import Data.Char (digitToInt)
import Data.List (transpose)
import Debug.Trace (trace)
import GHC.IO.Handle (hGetContents)
import Numeric (showIntAtBase)
import System.IO
import Text.Printf (printf)

example = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

xor' :: Int -> Int
xor' 1 = 0
xor' 0 = 1

commonValue :: [Int] -> Int
commonValue value = if oneCount >= totalLength / 2 then 1 else 0
  where
    oneCount = fromIntegral (sum value) :: Double
    totalLength = fromIntegral (length value) :: Double

toDec :: [Int] -> Int
toDec = foldl (\acc x -> acc * 2 + x) 0

parseReading :: String -> [Int]
parseReading = map digitToInt

part1 :: [String] -> Int
part1 readings = toDec gamma * toDec epsilon
  where
    gamma = (map (commonValue . parseReading) . transpose) readings
    epsilon = map xor' gamma

createOxygenPattern :: [[Int]] -> [[(Int, Int)]]
createOxygenPattern readings = map (zip pattern) readings
  where
    pattern = (map commonValue . transpose) readings

createCo2Pattern readings = map (zip pattern) readings
  where
    pattern = (map (xor' . commonValue) . transpose) readings

doesReadingMatch :: (Int, Int) -> Bool
doesReadingMatch (a, b) = a == b

findLifeSupportReading :: ([[Int]] -> [[(Int, Int)]]) -> [[Int]] -> Int -> [Int]
-- findLifeSupportReading createPattern a b | trace ("findLifeSupportReading " ++ show a ++ " " ++ show b) False = undefined
findLifeSupportReading createPattern [v] _ = v
findLifeSupportReading createPattern readings index =
  findLifeSupportReading
    createPattern
    ((map (map snd) . filter (doesReadingMatch . head . drop index)) (createPattern readings))
    (index + 1)

-- part2 :: [String] -> Int
part2 r = toDec oxygen * toDec co2
  where
    readings = map parseReading r
    oxygen = findLifeSupportReading createOxygenPattern readings 0
    co2 = findLifeSupportReading createCo2Pattern readings 0

main = do
  contents <- readFile "src/day3/input.txt"
  let l = lines contents
      answer = part1 l
      answer2 = part2 l
  print answer
  print answer2
