{-# LANGUAGE OverloadedStrings #-}

import Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Maybe

data Sue = Sue { number :: Int
               , features :: Map BS.ByteString Int
               } deriving (Show)

parseSue = do
  string "Sue"
  space
  i <- decimal
  char ':'
  space
  fs <- parseFeatures
  return $ Sue i (Map.fromList fs)

parseFeatures = parseFeature `sepBy` ", "

parseFeature = do
  f <- some letter_ascii
  char ':'
  space
  i <- decimal
  return (BS.pack f,i)

main = do
  input <- BS.getContents
  let sues = fromJust $ maybeResult $ parse (parseSue `sepBy` endOfLine) input
  print $ Prelude.filter (matchesSue . features) sues
  print $ Prelude.filter (matchesRealSue . features) sues
    where
      matchesSue m = Map.size m == Map.size (differenceWith compFun m sue)
      compFun a b = if a == b then Just a else Nothing
      matchesRealSue m = Map.size m == Map.size (differenceWithKey realCompFun m sue)
      realCompFun k a b = if realComp k a b then Just a else Nothing
      realComp k
        | k == "cats" || k == "trees" = (>)
        | k == "pomeranians" || k == "goldfish" = (<)
        | otherwise = (==)
      



sue = Map.fromList [("children", 3), ("cats", 7), ("samoyeds", 2),
    ("pomeranians", 3), ("akitas", 0), ("vizslas", 0), ("goldfish", 5),
    ("trees", 3), ("cars", 2), ("perfumes", 1)]
