{-# LANGUAGE OverloadedStrings #-}

import Data.Int
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
import Control.Monad
import Control.Applicative

type Gate = BS.ByteString

data VG
  = G Gate
  | V Int
  deriving (Show)

data GATE
  = APPLY VG Gate
  | NOT VG Gate
  | AND VG VG Gate
  | OR VG VG Gate
  | LSHIFT VG Int8 Gate 
  | RSHIFT VG Int8 Gate
  deriving (Show)

parseGate = takeTill isSpace >>= return
parseVal = decimal >>= (return . V)
parseVG = parseVal <|> (takeTill isSpace >>= return . G)


arrow = string " -> "

parseApply = do
  vg <- parseVG
  arrow
  g <- parseGate
  return $ APPLY vg g

parseNot = do
  string "NOT "
  g1 <- parseVG
  arrow
  g2 <- parseGate
  return $ NOT g1 g2

parseBinary str op = do
  i1 <- parseVG
  string str
  i2 <- parseVG
  arrow
  o <- parseGate
  return $ op i1 i2 o

parseAnd = parseBinary " AND " AND
parseOr = parseBinary " OR " OR

parseShift str op = do
  i <- parseVG
  string str
  s <- decimal
  arrow
  o <- parseGate
  return $ op i s o

parseShiftL = parseShift " LSHIFT " LSHIFT
parseShiftR = parseShift " RSHIFT " RSHIFT

parseCommand = parseApply <|> parseNot <|> parseAnd <|> parseOr
  <|> parseShiftL <|> parseShiftR

parser = parseCommand `sepBy` char '\n'

main = do
  input <- BS.getContents
  print $ parseOnly parser input

