import Text.Regex.Posix

main = do
  input <- getContents
  print $ length $ filter (True ==) $ map nice $ lines input
  print $ length $ filter (True ==) $ map nice' $ lines input

nice s = nice1 s && nice2 s && nice3 s

nice' s = nice4 s && nice5 s

nice1 = flip (=~) "[aeiou].*[aeiou].*[aeiou]"

nice2 = flip (=~) "(.)\\1"

nice3 = not . flip (=~) "(ab|cd|pq|xy)"

nice4 = flip (=~) "(..).*\\1"

nice5 = flip (=~) "(.).\\1"
