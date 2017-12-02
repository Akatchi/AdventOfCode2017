import System.IO
import Data.Char

nextItemInList :: Int -> [Int] -> Int
nextItemInList index list = list !! ((index + 1) `mod` (length list))

solveCaptcha :: [Int] -> Int
solveCaptcha list = sum [calculateCaptchaInput (nextItemInList a list) b | (a, b) <- zip [0..] list]

calculateCaptchaInput :: Int -> Int -> Int
calculateCaptchaInput a b = if a == b then a else 0

toIntArray :: String -> [Int]
toIntArray string = map digitToInt string

readFromFile :: FilePath -> IO [Int]
readFromFile = fmap toIntArray . readFile

main = do
  print("TestA should be 3: " ++ (show $ solveCaptcha [1, 1, 2, 2] == 3))
  print("TestB should be 4: " ++ (show $ solveCaptcha [1, 1, 1, 1] == 4))
  print("TestC should be 0: " ++ (show $ solveCaptcha [1, 2, 3, 4] == 0))
  print("TestD should be 9: " ++ (show $ solveCaptcha [9, 1, 2, 1, 2, 1, 2, 9] == 9))

  input <- readFromFile "input"
  print ("Answer: " ++ (show $ solveCaptcha input))
