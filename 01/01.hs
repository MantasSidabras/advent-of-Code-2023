import Data.Char (digitToInt, isDigit)
import System.IO

firstDigit :: String -> Char
firstDigit (x : xs) =
  if isDigit x
    then x
    else firstDigit xs

digitPair :: String -> (Char, Char)
digitPair xs =
  (firstDigit xs, firstDigit $ reverse xs)

calibrationValue :: (Char, Char) -> Int
calibrationValue (a, b) = read (a : [b])

main = do
  fileHandle <- openFile "input_1.txt" ReadMode
  content <- hGetContents fileHandle
  let linesContent = lines content
  let results = sum $ map (calibrationValue . digitPair) linesContent
  print results
