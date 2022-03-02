import Control.Monad ()
import qualified Data.Char as Char
import System.IO
  ( IOMode (ReadMode),
    hClose,
    hGetContents,
    openFile,
  )

example = ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

capitalized :: String -> String
capitalized (head : tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

data Direction = Forward | Down | Up deriving (Read, Show)

type Command = (Direction, Int)

type Position = (Int, Int, Int)

readCommand :: String -> Command
readCommand command = ((read . capitalized) c :: Direction, read amount :: Int)
  where
    [c, amount] = words command

move :: Position -> Command -> Position
move (x, y, aim) (Down, a) = (x, y, aim + a)
move (x, y, aim) (Up, a) = (x, y, aim - a)
move (x, y, aim) (Forward, a) = (x + a, y + aim * a, aim)

part2 input = x * y
  where
    commands = map readCommand input
    (x, y, aim) = foldl move (0, 0, 0) commands

moveX :: Int -> Command -> Int
moveX position (Forward, amount) = position + amount
moveX position _ = position

moveY :: Int -> Command -> Int
moveY position (Up, amount) = position - amount
moveY position (Down, amount) = position + amount
moveY position _ = position

part1 input = forward * depth
  where
    commands = map readCommand input
    forward = foldl moveX 0 commands
    depth = foldl moveY 0 commands

main = do
  let list = []
  handle <- openFile "src/day2/input.txt" ReadMode
  contents <- hGetContents handle
  let l = lines contents
      answer = part1 l
      answer2 = part2 l
  print answer
  print answer2
  hClose handle