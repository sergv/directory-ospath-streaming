-- |
-- Module:     BenchMain
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module BenchMain (main) where

import System.Directory.OsPath
import System.Directory.OsPath.Streaming
import Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bench "getDirectoryContentsRecursive from home directory" $
    nfIO $ getHomeDirectory >>= fmap length . getDirectoryContentsRecursive
  ]
