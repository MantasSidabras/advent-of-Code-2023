import Data.Char (digitToInt, isDigit)
import System.IO

(redLimit, greenLimit, blueLimit) = (12, 13, 14) :: (Int, Int, Int)

splitOn :: (Char -> Bool) -> String -> [String]
splitOn p s = case dropWhile p s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'


parseCardId :: String -> Int
parseCardId = read . unwords . take 1 . drop 1 . words

parseElements = map read . splitOn (== ' ')

parseValues :: String -> ([Int], [Int])
parseValues = (\parts -> (parseElements (head parts), parseElements (last parts))) . splitOn (== '|')

parseInput :: String -> Card
parseInput = (\[left, right] -> (parseCardId left, parseValues right)) . splitOn (== ':')

type Card = (Int, ([Int], [Int]))
calculateCard :: Card -> Int
calculateCard (_, (win, values)) =
  foldl
    ( \acc val ->
        if val `elem` win
          then case acc of
            0 -> 1
            _ -> acc * 2
          else acc
    )
    0
    values
  
main = do
  fileHandle <- openFile "input_1 .txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let cards = map parseInput linesContent
  let results = sum $ map calculateCard cards
  print results
