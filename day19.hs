{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Map (Map,(!))
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

parseElem = do
  f <- satisfy (\c -> isUpper c || ('e' == c))
  m <- many $ satisfy isLower
  return $ BS.pack $ f : m

parseReplacement = do
  elem <- parseElem
  string " => "
  repl <- some parseElem 
  return (elem,repl)

parseInput = do
  repls <- parseReplacement `sepBy` char '\n'
  many space
  molecules <- some parseElem
  return (repls, molecules)

toReplMap ((f,t):xs) = Map.insertWith (++) f [t] (toReplMap xs) 
toReplMap [] = Map.empty

main = do
  input <- BS.getContents
  let parsed = fromJust $ maybeResult $ parse parseInput input
  print $ length $ nub $ uncurry replaceOne $ mapFst toReplMap parsed
  print $ uncurry revReplace parsed
  print $ uncurry reverseMolecule parsed

mapFst f (a,b) = (f a, b)

replaceHead :: Map ByteString [[ByteString]] -> [ByteString] -> [[ByteString]]
replaceHead r (x:xs) = map (++ xs) (Map.findWithDefault [] x r)
replaceHead _ [] = []

replaceOne :: Map ByteString [[ByteString]] -> [ByteString] -> [[ByteString]]
replaceOne r a@(x:xs) = replaceHead r a ++ ((x :) <$> replaceOne r xs)
replaceOne _ [] = []

reverseMolecule :: [(ByteString, [ByteString])] -> [ByteString] -> Int
reverseMolecule repls l =
  if l == ["e"] 
    then 0
    else 1 + reverseMolecule repls (revReplace repls l)

revReplace :: [(ByteString, [ByteString])] -> [ByteString] -> [ByteString]
revReplace repls l = concat $ catMaybes $ concatMap (uncurry (revReplaceOne l)) (fmap swap repls)

revReplaceOne :: [ByteString] -> [ByteString] -> ByteString
    -> [Maybe [ByteString]]
revReplaceOne l@(x:xs) f t = 
    revReplaceHead f t l : (liftM (x :) <$> revReplaceOne xs f t)
revReplaceOne [] _ _ = [Nothing]

revReplaceHead :: [ByteString] -> ByteString -> [ByteString]
    -> Maybe [ByteString]
revReplaceHead f t l = liftM (t :) (stripPrefix f l)
