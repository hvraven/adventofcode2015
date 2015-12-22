import Text.Regex.Posix
import Data.Char
import Data.List

incPassword :: String -> String
incPassword [] = "a"
incPassword p
  | last p == 'z' = incPassword (init p) ++ "a"
  | otherwise = init p ++ [chr $ 1 + ord (last p)]

validPassword :: String -> Bool
validPassword p = check2 p && check1 p && check3 p
  where
    check1 (a:b:c:xs) = elem [a,b,c] (map (map (chr . (ord 'a' +))) [[i,i+1,i+2] | i <- [0..23] ]) || check1 (b:c:xs)
    check1 _ = False
    check2 = not . any (\a -> a == 'i' || a == 'o' || a == 'l')
    check3 = flip (=~) "(.)\\1.*(.)\\2"

passwords = filter validPassword . iterate incPassword

main = print $ take 2 $ passwords "hxbxwxba"


