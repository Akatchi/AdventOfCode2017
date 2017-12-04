import System.IO
import Data.List

parseLine :: String -> [[String]]
parseLine line = fmap words $ lines line

readFromFile :: FilePath -> IO [[String]]
readFromFile = fmap parseLine . readFile

isValid :: [String] -> Bool
isValid pwdList = length sortedPwdList == length (nub sortedPwdList)
  where sortedPwdList = map sort pwdList

countTrues :: [Bool] -> Int
countTrues x = length [ x | x <- x, x == True]

main = do
  print("TestA should be True: " ++ (show $ isValid ["abcde", "fghij"]))
  print("TestB should be False: " ++ (show $ isValid ["abcde", "xyz", "ecdab"]))
  print("TestC should be True: " ++ (show $ isValid ["a", "ab", "abc", "abd", "abf", "abj"]))
  print("TestD should be True: " ++ (show $ isValid ["iiii", "oiii", "ooii", "oooi", "oooo"]))
  print("TestE should be False: " ++ (show $ isValid ["oiii", "ioii", "iioi", "iiio"]))

  input <- readFromFile "input"

  let validPwds = countTrues (map isValid input)

  print ("Valid password counts: " ++ (show validPwds))