{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Map ((!))
import qualified Data.Map.Strict as Map
import Data.Attoparsec.ByteString.Char8
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe
import Data.List
import Data.Tuple

parseName = takeTill isSpace

parseDistance = do
  from <- parseName
  string " to "
  to <- parseName
  string " = " 
  distance <- decimal
  return (from, to, distance)

parser = parseDistance `sepBy` endOfLine

makeNodes :: [(BS.ByteString,BS.ByteString,Int)] -> [LNode BS.ByteString]
makeNodes = zip [1..] . nub . concatMap getNodes
  where getNodes (a,b,_) = [a,b]

makeEdges :: [(BS.ByteString,BS.ByteString,Int)] -> [LNode BS.ByteString]
  -> [LEdge Int]
makeEdges p nodes = addReverse $ map makeEdge p
  where
    addReverse ((l,r,b):xs) = (l,r,b):(r,l,b):addReverse xs
    addReverse [] = []
    makeEdge (from,to,dist) = (nodeMap ! from, nodeMap ! to, dist)
    nodeMap = Map.fromList $ map swap nodes

main = do
  input <- BS.getContents
  let p = fromJust $ maybeResult $ parse parser input
  let nodes = makeNodes p
  let edges = makeEdges p nodes
  let g = mkGraph nodes edges :: Gr BS.ByteString Int
  let routes = permutations $ map fst nodes
  print $ minimum $ map (routeLength g) routes
  print $ maximum $ map (routeLength g) routes

routeLength g (x:y:xs) = Map.fromList (lsuc g x) ! y + routeLength g (y:xs)
routeLength _ [_] = 0
routeLength _ [] = 0
