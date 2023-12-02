import System.IO

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'

type GameId = Int

data Color = Red | Green | Blue deriving (Show, Eq)

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

minColorCube :: Color -> [Cube] -> Cube
minColorCube color =
  (\(x) -> (color, x))
    . foldl
      ( \acc (c, value) -> if c == color then max acc value else acc
      )
      0

maxCubes :: [Cube] -> (Cube, Cube, Cube)
maxCubes xs = (minColorCube Red xs, minColorCube Green xs, minColorCube Blue xs)

gameBestCubes :: Game -> (GameId, (Cube, Cube, Cube))
gameBestCubes (gameId, games) =
  let list = concat games
   in (gameId, maxCubes list)

gamePower :: Game -> Int
gamePower = (\(gameId, ((_, r), (_, g), (_, b))) -> r * g * b) . gameBestCubes

main = do
  fileHandle <- openFile "input_1.txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let games = map parseInput linesContent
  let results = sum $ map gamePower games
  print results

-- mapM_ print results
