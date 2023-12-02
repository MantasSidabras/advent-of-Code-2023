import Data.Char (digitToInt, isDigit)
import Data.List (find, isInfixOf, isPrefixOf)
import System.IO

digits = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitsMap :: [(String, Char)]
digitsMap = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

findDigit :: String -> Char
findDigit [x] = x
findDigit xs =
  let res = find (\(text, val) -> text == xs) digitsMap
   in case res of
        Just (_, val) -> val
        Nothing -> error "this should not happen"

firstDigit :: String -> [String] -> String
firstDigit [] dict = error "oh no"
firstDigit xs dict =
  let (x, fnd) = foldr (\digit (acc, found) -> if digit `isPrefixOf` xs && not found then (digit, True) else (acc, found)) ("", False) dict
   in if fnd then x else firstDigit (tail xs) dict

digitPair :: String -> (Char, Char)
digitPair xs =
  (findDigit $ firstDigit xs digits, findDigit $ reverse $ firstDigit (reverse xs) (map reverse digits))

calibrationValue :: (Char, Char) -> Int
calibrationValue (a, b) = read (a : [b])

main = do
  fileHandle <- openFile "input_1.txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let results = sum $ map (calibrationValue . digitPair) linesContent
  print results
