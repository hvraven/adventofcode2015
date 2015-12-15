import Data.List (nub)
import Control.Arrow ((***))
import Control.Monad (join)

main = do
  input <- getContents
  print $ part1 input
  print $ part2 input

part1 = length . nub . santaWalk

part2 = length . nub . concat . join (***) santaWalk . split

split = unzip . split'
  where
    split' (x:y:xs) = (x,y) : split' xs
    split' [] = []

santaWalk = santaWalk' (0,0)
  where
    santaWalk' (x,y) (c:cs)
      | c == '^' = (x,y) : santaWalk' (x,y+1) cs
      | c == 'v' = (x,y) : santaWalk' (x,y-1) cs
      | c == '<' = (x,y) : santaWalk' (x-1,y) cs
      | c == '>' = (x,y) : santaWalk' (x+1,y) cs
    santaWalk' end [] = [end]
