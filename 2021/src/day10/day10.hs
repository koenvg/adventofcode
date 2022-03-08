import Control.Monad (foldM)
import Data.Either (isLeft, lefts, rights)
import Data.List (sort)

example = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"

type ChunkType = Char

data Chunk = Chunk {chunkType :: ChunkType, children :: [Chunk]} deriving (Show)

median :: [Int] -> Int
median list =
  let middle = fromIntegral (length list) / 2
   in list !! floor middle

closeChunk :: [Chunk] -> Char -> Either Char [Chunk]
closeChunk chunks c
  | null chunks = Left c
  | chunkType (head chunks) /= c = Left c
  | length chunks == 1 = Right []
  | otherwise =
    let (a : b : xs) = chunks
        newChunks = b {children = a : (children b)} : xs
     in Right newChunks

parse = foldM (step) []
  where
    step :: [Chunk] -> Char -> Either Char [Chunk]
    step acc c = case c of
      '{' -> Right $ Chunk {chunkType = '}', children = []} : acc
      '<' -> Right $ Chunk {chunkType = '>', children = []} : acc
      '(' -> Right $ Chunk {chunkType = ')', children = []} : acc
      '[' -> Right $ Chunk {chunkType = ']', children = []} : acc
      _ -> closeChunk acc c

scorePart1 :: Char -> Int
scorePart1 ')' = 3
scorePart1 ']' = 57
scorePart1 '}' = 1197
scorePart1 '>' = 25137
scorePart1 c = 0

scorePart2 :: Char -> Int
scorePart2 ')' = 1
scorePart2 ']' = 2
scorePart2 '}' = 3
scorePart2 '>' = 4
scorePart2 c = 0

part1 = sum . map scorePart1 . lefts . map parse . lines

closeLine :: [Chunk] -> String
closeLine = map chunkType

calculateScore :: Int -> Char -> Int
calculateScore acc c = acc * 5 + scorePart2 c

part2 input =
  let scores = (map (foldl calculateScore 0 . closeLine) . rights . map parse . lines) input
   in median $ sort scores

main = do
  content <- readFile "src/day10/input.txt"
  let a1 = part1 content
      a2 = part2 content
  print a1
  print a2