{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Char
import Control.Applicative
import Data.Maybe
import Data.List

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
  return (toReplMap repls, molecules)
    where
      toReplMap ((f,t):xs) = Map.insertWith (++) f [t] (toReplMap xs) 
      toReplMap [] = Map.empty

main = do
  input <- BS.getContents
  let parsed = fromJust $ maybeResult $ parse parseInput input
  print $ length $ nub $ uncurry replaceOne parsed

replaceHead :: Map ByteString [[ByteString]] -> [ByteString] -> [[ByteString]]
replaceHead r (x:xs) = map (++ xs) (Map.findWithDefault [] x r)
replaceHead _ [] = []

replaceOne :: Map ByteString [[ByteString]] -> [ByteString] -> [[ByteString]]
replaceOne r a@(x:xs) = replaceHead r a ++ ((x :) <$> replaceOne r xs)
replaceOne _ [] = []
