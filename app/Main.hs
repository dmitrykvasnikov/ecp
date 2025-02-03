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
  dirs <- listDirectory "."
  files <- findFiles dirs "name.key"
  list <- traverse convertFile files
  let content = intercalate "\n" list
  writeFile "output.csv" content
  putStrLn "Done!"

convertFile :: FilePath -> IO String
convertFile file = do
  content <- (dropWhile (not . isAlpha) . LBS.toString . convert "CP1251" "UTF-8" . BS.fromStrict) <$> BS.readFile file
  let (n : d' : _) = splitOn "_" content
  let d = intercalate "/" $ splitOn "-" d'
  pure (n <> "," <> d)
