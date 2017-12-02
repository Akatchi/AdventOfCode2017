import System.IO
import Data.Char

nextItemInList :: Int -> [Int] -> Int
nextItemInList index list = list !! ((index + 1) `mod` (length list))

getHalveTheSize :: [Int] -> Int
getHalveTheSize list = (length list) `div` 2

nextItemInListPartB :: Int -> [Int] -> Int
nextItemInListPartB index list = nextItemInList ((index - 1) + getHalveTheSize list) list

getListWithIndex :: [Int] -> Int
getListWithIndex list = sum [doMagic (nextItemInListPartB a list) b | (a, b) <- zip [0..] list]

doMagic :: Int -> Int -> Int
doMagic a b = if a == b then a else 0

toIntArray :: String -> [Int]
toIntArray string = map digitToInt string

readFromFile :: FilePath -> IO [Int]
readFromFile = fmap toIntArray . readFile

main = do
  print("TestA should be 6: " ++ (show $ getListWithIndex [1, 2, 1, 2] == 6))
  print("TestB should be 0: " ++ (show $ getListWithIndex [1, 2, 2, 1] == 0))
  print("TestC should be 4: " ++ (show $ getListWithIndex [1, 2, 3, 4, 2, 5] == 4))
  print("TestD should be 12: " ++ (show $ getListWithIndex [1, 2, 3, 1, 2, 3] == 12))
  print("TestE should be 4: " ++ (show $ getListWithIndex [1, 2, 1, 3, 1, 4, 1, 5] == 4))

  input <- readFromFile "input"
  print ("Answer: " ++ (show $ getListWithIndex input))
