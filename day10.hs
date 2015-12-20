import Data.List
import Data.Char

lookAndSay :: [Int] -> [Int]
lookAndSay = concatMap say . group
  where
    say i = length i : [head i]

main = do
  input <- getContents
  print $ length (iterate lookAndSay (map digitToInt input) !! 40)
  print $ length (iterate lookAndSay (map digitToInt input) !! 50)
