-- |
-- Module:     TestMain
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestMain (main) where

import Control.Exception
import Data.Functor.Identity (Identity(..))
import qualified Data.List as L
import System.OsPath

import System.Directory.OsPath.Streaming
import System.Directory.OsPath.Types

import Test.Tasty
import Test.Tasty.HUnit

#ifndef mingw32_HOST_OS
import Numeric (showHex)
import System.Directory.OsPath
import System.OsString.Internal.Types (getOsString)
import System.Random

import qualified System.Posix.Files.PosixString as Posix
#endif

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "readDirStream" $ do
      res <- bracket (openDirStream [osp|test/filesystem|]) closeDirStream $ \ds -> do
        Just w <- readDirStream ds
        Just x <- readDirStream ds
        Just y <- readDirStream ds
        Just z <- readDirStream ds
        return $ L.sort [w, x, y, z]
      res @?= [([osp|bar.txt|], File Regular), ([osp|baz.txt|], File Regular), ([osp|bin|], Directory Regular), ([osp|foo.txt|], File Regular)]

  , testCase "readDirStreamFull" $ do
      let dir = [osp|test/filesystem|]
      res <- bracket (openDirStream dir) closeDirStream $ \ds -> do
        Just w <- readDirStreamFull ds
        Just x <- readDirStreamFull ds
        Just y <- readDirStreamFull ds
        Just z <- readDirStreamFull ds
        return $ L.sort [w, x, y, z]
      res @?= map
        (\(x, y) -> (dir </> x, Basename x, y))
        [([osp|bar.txt|], File Regular), ([osp|baz.txt|], File Regular), ([osp|bin|], Directory Regular), ([osp|foo.txt|], File Regular)]

  , testGroup "getFileType general"
      [ testCase "file" $ do
          getFileType [osp|directory-ospath-streaming.cabal|] >>= (@?= File Regular)
      , testCase "directory" $ do
          getFileType [osp|test|] >>= (@?= Directory Regular)
      ]

  , testGroup "contents"
      [ testCase "getDirectoryContentsRecursive" $ do
          res <- L.sort <$> getDirectoryContentsRecursive [osp|test/filesystem|]
          res @?= [([osp|bar.txt|], File Regular), ([osp|baz.txt|], File Regular), ([osp|bin|], Directory Regular), ([osp|bin|] </> [osp|bin.txt|], File Regular), ([osp|foo.txt|], File Regular)]

      , testCase "getDirectoryContentsWithFilterRecursive 1" $ do
          res <- L.sort <$> getDirectoryContentsWithFilterRecursive (\_ _ -> True) (const True) [osp|test/filesystem|]
          res @?= [([osp|bar.txt|], File Regular), ([osp|baz.txt|], File Regular), ([osp|bin|], Directory Regular), ([osp|bin|] </> [osp|bin.txt|], File Regular), ([osp|foo.txt|], File Regular)]

      , testCase "getDirectoryContentsWithFilterRecursive 2" $ do
          res <- L.sort <$> getDirectoryContentsWithFilterRecursive (\x _ -> x /= Basename [osp|bin|]) (const True) [osp|test/filesystem|]
          res @?= [([osp|bar.txt|], File Regular), ([osp|baz.txt|], File Regular), ([osp|bin|], Directory Regular), ([osp|foo.txt|], File Regular)]

      , testCase "getDirectoryContentsWithFilterRecursive 3" $ do
          res <- L.sort <$> getDirectoryContentsWithFilterRecursive (\_ _ -> True) (`elem` [Basename [osp|foo.txt|], Basename [osp|bin|], Basename [osp|bin.txt|]]) [osp|test/filesystem|]
          res @?= [([osp|bin|], Directory Regular), ([osp|bin|] </> [osp|bin.txt|], File Regular), ([osp|foo.txt|], File Regular)]

      , testCase "listContentsRecFold" $ do
          let dir = [osp|test/filesystem|]
          res <- fmap L.sort $
            listContentsRecFold
              Nothing
              (\a b c d e cons rec rest -> cons (a, b, c, d, Directory e) $ rec rest)
              (\a b c d e -> pure $ Just (a, b, c, d, e))
              (Identity dir)
          res @?=
            [ (dir </> [osp|bar.txt|],                dir, Relative [osp|bar.txt|],                  Basename [osp|bar.txt|], File Regular)
            , (dir </> [osp|baz.txt|],                dir, Relative [osp|baz.txt|],                  Basename [osp|baz.txt|], File Regular)
            , (dir </> [osp|bin|],                    dir, Relative [osp|bin|],                      Basename [osp|bin|],     Directory Regular)
            , (dir </> [osp|bin|] </> [osp|bin.txt|], dir, Relative $ [osp|bin|] </> [osp|bin.txt|], Basename [osp|bin.txt|], File Regular)
            , (dir </> [osp|foo.txt|],                dir, Relative [osp|foo.txt|],                  Basename [osp|foo.txt|], File Regular)
            ]
      ]

#ifndef mingw32_HOST_OS
  , withResource
      (do
        tmp <- getTemporaryDirectory >>= canonicalizePath
        createFreshTempDir tmp [osp|test|])
      removeDirectoryRecursive
    $ \mkTmpDir -> testGroup "getFileType unix"
        [ testCase "file symlink" $ do
            tmp     <- mkTmpDir
            currDir <- getCurrentDirectory
            let dest = tmp </> [osp|tmp1|]
            Posix.createSymbolicLink
              (getOsString (currDir </> [osp|directory-ospath-streaming.cabal|]))
              (getOsString dest)
            ft <- getFileType dest
            ft @?= File Symlink
        , testCase "directory symlink" $ do
            tmp     <- mkTmpDir
            currDir <- getCurrentDirectory
            let dest = tmp </> [osp|tmp2|]
            Posix.createSymbolicLink
              (getOsString (currDir </> [osp|src|]))
              (getOsString dest)
            ft <- getFileType dest
            ft @?= Directory Symlink
        , testCase "other" $ do
            tmp <- mkTmpDir
            let dest = tmp </> [osp|tmp3|]
            res <- tryIO $ Posix.createNamedPipe (getOsString dest) 0
            case res of
              -- Creating named pipe might fail on some filesystems
              Left _  -> pure ()
              Right _ -> do
                ft <- getFileType dest
                ft @?= Other Regular
        , testCase "recursive symlink is other" $ do
            tmp <- mkTmpDir
            let dest = tmp </> [osp|tmp4|]
            Posix.createSymbolicLink
              (getOsString dest)
              (getOsString dest)
            ft <- getFileType dest
            ft @?= Other Symlink
        , testCase "dangling symlink is other" $ do
            tmp <- mkTmpDir
            let dest = tmp </> [osp|tmp5|]
            Posix.createSymbolicLink
              (getOsString (tmp </> [osp|does-not-exist|]))
              (getOsString dest)
            ft <- getFileType dest
            ft @?= Other Symlink
        ]
#endif
  ]

#ifndef mingw32_HOST_OS
tryIO :: IO a -> IO (Either IOException a)
tryIO = try

createFreshTempDir :: OsPath -> OsPath -> IO OsPath
createFreshTempDir dir prefix = go
  where
    go = do
      (n :: Word) <- randomIO
      n'          <- encodeUtf (showHex n [])
      let path = dir </> prefix <> [osp|-|] <> n'
      exists <- doesDirectoryExist path
      if exists
        then go
        else do
          createDirectory path
          pure path
#endif
