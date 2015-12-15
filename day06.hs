{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Maybe

coordsParser = do
  x1 <- decimal
  char ','
  y1 <- decimal
  string " through "
  x2 <- decimal
  char ','
  y2 <- decimal
  return ((x1,y1),(x2,y2))

parseOn = do
  string "turn on "
  c <- coordsParser
  return $ ON c

parseOff = do
  string "turn off "
  c <- coordsParser
  return $ OFF c
  
parseToggle = do
  string "toggle "
  c <- coordsParser
  return $ TOGGLE c

parserCommand :: Parser Instruction
parserCommand = parseOn <|> parseOff <|> parseToggle

parser :: Parser [Instruction]
parser = parserCommand `sepBy` char '\n'

data Instruction
  = ON ((Int, Int),(Int,Int))
  | OFF ((Int, Int),(Int,Int))
  | TOGGLE ((Int, Int),(Int,Int))
  deriving (Show)

type Coord = ((Int,Int),(Int,Int))
type Grid = (Int,Int) -> Bool
type BrightnessGrid = (Int,Int) -> Int

makeGrid = foldl translateCommand start
  where
    translateCommand g (ON c) = on c g
    translateCommand g (OFF c) = off c g
    translateCommand g (TOGGLE c) = toggle c g

makeBrightnessGrid = foldl translateCommand bstart
  where
    translateCommand g (ON c) = step 1 c g
    translateCommand g (OFF c) = step (-1) c g
    translateCommand g (TOGGLE c) = step 2 c g

main = do
  input <- BS.getContents
  let commands = fromJust $ maybeResult $ parse parser input
  let grid = makeGrid commands
  let bgrid = makeBrightnessGrid commands
  -- part 1
  print $ length [(x,y) | x <- [0..999], y <- [0..999], grid (x,y)]
  print $ sum $ map bgrid [(x,y) | x <- [0..999], y <- [0..999]]

start _ = False
bstart _ = 0

on :: Coord -> Grid -> Grid
on ((x1,y1),(x2,y2)) f (x,y) =
  if x1 <= x && x <= x2 && y1 <= y && y <= y2
    then True
    else f (x,y)

off ((x1,y1),(x2,y2)) f (x,y) =
  if x1 <= x && x <= x2 && y1 <= y && y <= y2
    then False
    else f (x,y)

toggle ((x1,y1),(x2,y2)) f (x,y) =
  if x1 <= x && x <= x2 && y1 <= y && y <= y2
    then not (f (x,y))
    else f (x,y)

step :: Int -> Coord -> BrightnessGrid -> BrightnessGrid
step i ((x1,y1),(x2,y2)) f (x,y) =
  if x1 <= x && x <= x2 && y1 <= y && y <= y2
    then maximum [i + f (x,y),0]
    else f (x,y)
