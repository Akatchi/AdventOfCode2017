import System.IO
import Data.List

parseLine :: String -> [[String]]
parseLine line = fmap words $ lines line

readFromFile :: FilePath -> IO [[String]]
readFromFile = fmap parseLine . readFile

isValid :: [String] -> Bool
isValid pwdList = length pwdList == length (nub pwdList)

countTrues :: [Bool] -> Int
countTrues x = length [ x | x <- x, x == True]

main = do
  print("TestA should be True: " ++ (show $ isValid ["aa", "bb", "cc", "dd", "ee"]))
  print("TestB should be False: " ++ (show $ isValid ["aa", "bb", "cc", "dd", "aa"]))
  print("TestC should be True: " ++ (show $ isValid ["aa", "bb", "cc", "dd", "aaa"]))

  input <- readFromFile "input"

  let validPwds = countTrues (map isValid input)

  print ("Valid password counts: " ++ (show validPwds))