import System.IO
import Data.Char


checksum :: [Int] -> Int
checksum [] = 0
checksum x = (maximum x) - (minimum x)

stringToIntList :: String -> [Int]
stringToIntList x = map read $ words x :: [Int]

parseLine :: String -> [[Int]]
parseLine line = fmap stringToIntList (lines line)

readFromFile :: FilePath -> IO [[Int]]
readFromFile = fmap parseLine . readFile

main = do
  print("TestA should be 8: " ++ (show $ checksum [5, 1, 9, 5] == 8))
  print("TestB should be 4: " ++ (show $ checksum [7, 5, 3] == 4))
  print("TestC should be 6: " ++ (show $ checksum [2, 4, 6, 8] == 6))

  input <- readFromFile "input"
  print ("Answer: " ++ (show (sum (map checksum input))))