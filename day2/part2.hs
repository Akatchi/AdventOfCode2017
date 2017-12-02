import System.IO
import Data.Char
import Data.List


checksum :: Integral a => [a] -> [a]
checksum [] = []
checksum (l:ls) = [maximum [l, x] `div` minimum [l, x] | x <- ls, l `mod` x == 0 || x `mod` l == 0] ++ checksum ls

stringToIntList :: String -> [Int]
stringToIntList x = map read $ words x :: [Int]

parseLine :: String -> [[Int]]
parseLine line = fmap stringToIntList (lines line)

readFromFile :: FilePath -> IO [[Int]]
readFromFile = fmap parseLine . readFile

main = do
  print("TestA should be [4]: " ++ (show $ checksum [5, 9, 2, 8] == [4]))
  print("TestB should be [3]: " ++ (show $ checksum [9, 4, 7, 3] == [3]))
  print("TestC should be [2]: " ++ (show $ checksum [3, 8, 6, 5] == [2]))

  input <- readFromFile "input"
  print ("Answer: " ++ (show (sum (concat (map checksum input)))))