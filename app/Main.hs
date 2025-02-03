{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Text.IConv
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy.UTF8 as LBS
import           Data.Char
import           Data.List
import           Data.List.Split
import           System.Directory

main :: IO ()
main = do
  -- Read the file as a ByteString
  bs <- (dropWhile (not . isAlpha) . LBS.toString . convert "CP1251" "UTF-8" . BS.fromStrict) <$> BS.readFile "name.key"
  let (n : d' : _) = splitOn "_" bs
  let d = intercalate "/" $ splitOn "-" d'
  putStrLn $ "Name: " <> n
  putStrLn $ "Date: " <> d
  writeFile "output.csv" (n <> "," <> d)
  dirs <- listDirectory "."
  files <- findFiles dirs "file1"
  mapM_ putStrLn files
  pure ()
