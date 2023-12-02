import Data.Char (digitToInt, isDigit)
import System.IO

(redLimit, greenLimit, blueLimit) = (12, 13, 14) :: (Int, Int, Int)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'

type GameId = Int

data Color = Red | Green | Blue deriving (Show)

type Cube = (Color, Int)

type Game = (GameId, [[Cube]])

trimStart :: String -> String
trimStart = dropWhile (== ' ')

parseGameId :: String -> Int
parseGameId = read . last . words

matchColor :: String -> Color
matchColor x
  | x == "red" = Red
  | x == "green" = Green
  | otherwise = Blue

parseCube :: String -> Cube
parseCube = (\[x, y] -> (matchColor y, read x)) . words . trimStart

parseCubes :: String -> [[Cube]]
parseCubes xs =
  map (map parseCube . splitOn (== ',')) $ splitOn (== ';') xs

parseInput :: String -> (GameId, [[Cube]])
parseInput xs =
  let [game, cubes] = splitOn (== ':') xs
   in (parseGameId game, parseCubes cubes)

isGoodCube :: Cube -> Bool
isGoodCube (Red, v) = v <= redLimit
isGoodCube (Green, v) = v <= greenLimit
isGoodCube (Blue, v) = v <= blueLimit

isGamePossible :: Game -> Bool
isGamePossible (id, games) = all (all isGoodCube) games

main = do
  fileHandle <- openFile "input_1.txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let games = map parseInput linesContent
  let results = sum . map fst $ filter isGamePossible games
  print results
