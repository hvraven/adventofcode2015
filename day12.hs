{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Control.Applicative

allDigits = skipWhile notSigned *> many (signed decimal <* skipWhile notSigned)
    where
      notSigned c = (c < '0' || c > '9') && c /= '-' && c /= '+'

main = do
  input <- BS.getContents
  print $ sum <$> parseOnly allDigits input
