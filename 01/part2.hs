main :: IO()
main = do
  input <- getLine
  putStrLn $ show $ find (-1) $ partialSum $ map transfBraces input

transfBraces :: Char -> Int
transfBraces '(' = 1
transfBraces ')' = -1
transfBraces _ = 0

partialSum :: [Int] -> [Int]
partialSum = pS 0
  where 
    pS i (x:xs) = (i+x) : pS (i+x) xs
    pS _ [] = []

find = find' 1
  where
    find' i s (x:xs) | s == x = i
                     | otherwise = find' (i+1) s xs
    find' _ _ [] = -1
