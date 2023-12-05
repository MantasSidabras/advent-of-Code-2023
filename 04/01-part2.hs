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
parseInput = (\list -> (parseCardId (head list), parseValues (last list))) . splitOn (== ':')

type Card = (Int, ([Int], [Int]))

type CardV = (Int, Int)

calculateCard :: Card -> Int
calculateCard (_, (win, values)) =
  foldl (\acc val -> if val `elem` win then acc + 1 else acc) 0 values

getCopies :: [CardV] -> CardV -> [CardV]
getCopies deck card =
  foldl (\acc deckCard -> if fst deckCard `elem` idxRange then acc ++ [deckCard] else acc) [] deck
  where
    index = fst card
    count = snd card
    end = index + count
    len = length deck
    idxRange = [index + 1 .. (min end len)]

processCards :: Int -> [CardV] -> [CardV] -> Int
processCards acc deck [] = acc
processCards acc deck copies = acc + length copies + rest
  where
    rest = sum $ map (processCards acc deck . getCopies deck) copies

main = do
  fileHandle <- openFile "input_1.txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let cards = map ((\card -> (fst card, calculateCard card)) . parseInput) linesContent
  let results = processCards 0 cards cards
  print results
