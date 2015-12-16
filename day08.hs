main = do
  input <- getContents
  let code = sum $ map length $ lines input
  let mem = sum $ map (length . transformEscapes) $ lines input
  let bloat = sum $ map (length . extraEscape) $ lines input
  print $ code - mem
  print $ bloat - code

transformEscapes ('"':xs) = transformEscapes xs
transformEscapes ('\\':'\"':xs) = '\"' : transformEscapes xs
transformEscapes ('\\':'\\':xs) = '\\' : transformEscapes xs
transformEscapes ('\\':'x':_:_:xs) = '_' : transformEscapes xs
transformEscapes (x:xs) = x : transformEscapes xs
transformEscapes [] = []

extraEscape xs = '\"' : extraEscape' xs ++ "\""

extraEscape' ('\"':xs) = "\\\"" ++ extraEscape' xs
extraEscape' ('\\':xs) = "\\\\" ++ extraEscape' xs
extraEscape' (x:xs) = x : extraEscape' xs
extraEscape' [] = []
