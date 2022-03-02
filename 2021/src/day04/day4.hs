import Control.Monad.State
import Data.Foldable (Foldable (fold))
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import GHC.Base (VecElem (Int16ElemRep))
import GHC.IO.Handle (NewlineMode (inputNL))
import GHC.Read (readField)

example = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n8  2 23  4 24\n21  9 14 16  7\n6 10  3 18  5\n1 12 20 15 19\n\n3 15  0  2 22\n9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n2  0 12  3  7"

data Square = Square {number :: Int, checked :: Bool} deriving (Show)

type Board = [[Square]]

parseList :: String -> [Int]
parseList = map read . splitOn ","

parse :: String -> ([Int], [Board])
parse s = (parseList (head parts), readBoards (tail parts))
  where
    parts = lines s

parseBoardLine :: String -> [Square]
parseBoardLine = map (\n -> Square {checked = False, number = read n :: Int}) . words

readBoards :: [String] -> [Board]
readBoards [] = []
readBoards ("" : r1 : r2 : r3 : r4 : r5 : xs) = map parseBoardLine [r1, r2, r3, r4, r5] : readBoards xs
readBoards _ = []

markSquare :: Int -> Square -> Square
markSquare a s = if a == number s then Square {number = number s, checked = True} else s

markBoardWith :: Int -> Board -> Board
markBoardWith a = map (map (markSquare a))

hasWon :: Board -> Bool
hasWon board = horizontal || vertical
  where
    horizontal = any (all (\Square {checked = c} -> c)) board
    vertical = any (all (\Square {checked = c} -> c)) (transpose board)

calculateScore :: Int -> Board -> Int
calculateScore lastNumber board = lastNumber * sumNotChecked
  where
    sumNotChecked =
      foldl
        (\acc Square {checked = c, number = n} -> if c then acc else acc + n)
        0
        (concat board)

play :: [Int] -> State [Board] Int
play [] = do
  return 0
play (x : xs) = do
  boards <- get
  let newBoards = map (markBoardWith x) boards
      winner = find hasWon newBoards
  put newBoards
  case winner of
    Nothing -> play xs
    Just w -> return (calculateScore x w)

type SquidState = ([Int], [Board])

play2 :: [Int] -> State SquidState [Int]
play2 [] = do
  (winningScores, _) <- get
  return winningScores
play2 (x : xs) = do
  (winningScores, playingBoards) <- get
  let newBoards = map (markBoardWith x) playingBoards
      thisRoundsWinners = (map (calculateScore x) . filter hasWon) newBoards
      newPlayingBoards = filter (not . hasWon) newBoards
  put (winningScores ++ thisRoundsWinners, newPlayingBoards)
  play2 xs

part1 input = do
  let (numbers, boards) = parse input
  let winning = evalState (play numbers) boards
  print winning

part2 input = do
  let (numbers, boards) = parse input
  let winners = evalState (play2 numbers) ([], boards)
  print (last winners)

main = do
  content <- readFile "src/day4/input.txt"
  part1 content
  part2 content