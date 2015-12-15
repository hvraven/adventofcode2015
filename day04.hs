{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Digest.Pure.MD5
import System.IO (stdout)
import Text.Read (read)
import Text.Show (show)
import Data.Tuple
import Data.Maybe
import Data.List

main = do
  secret <- BS.getContents
  print $ fmap (1+) (findIndex (\hash -> take 5 (show hash) == "00000") $ hashes secret)
  print $ fmap (1+) (findIndex (\hash -> take 6 (show hash) == "00000") $ hashes secret)

hashes secret = fmap (md5 . BS.append secret . BS.pack . show) [1..]
