import Data.List.Split (splitOn)

main = do
  input <- getContents
  print $ sum $ map (paperSize . (map read) . splitOn "x") $ lines input 

paperSize (l:w:h:[]) = 2*l*w+2*w*h+2*h*l+slack
  where
    slack = minimum [l*w,w*h,h*l]
