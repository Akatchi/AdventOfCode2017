import System.IO
import Data.Char

updateValue :: Int -> Int
updateValue x = if x >= 3 then x - 1 else x + 1

incrementValue :: [Int] -> Int -> [Int]
incrementValue input pos = (take pos input) ++ [updateValue $ input !! pos] ++ (drop (pos + 1) input)

move :: [Int] -> Int -> Int -> Int
move input cur_pos cur_steps
  | cur_pos >= (length input) = cur_steps
  | otherwise =
    let new_pos = cur_pos + (input !! cur_pos)
        new_steps = cur_steps + 1
        new_input = incrementValue input cur_pos
    in move new_input new_pos new_steps

toInt :: String -> Int
toInt x = read x :: Int

toIntArray :: String -> [Int]
toIntArray input = map toInt $ words input

readFromFile :: FilePath -> IO [Int]
readFromFile = fmap toIntArray . readFile

main = do
  print("TestA should be 10: " ++ (show $ move [0, 3, 0, 1, -3] 0 0 == 10))

  input <- readFromFile "input"
  print("Total steps required: " ++ (show $ move input 0 0))
