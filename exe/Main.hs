-- |
-- Module:     Main
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Function (on)
import qualified Data.List as L
import Data.Traversable (for)
import System.Exit (die)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import System.Directory.OsPath.Contents
import qualified System.Directory.OsPath.FileType as Streaming
import qualified System.Directory.OsPath.Streaming as Streaming
import qualified System.Directory.OsPath.SafeStreaming as SafeStreaming

getDirectoryContentsRecursiveStrict :: OsPath -> IO [OsPath]
getDirectoryContentsRecursiveStrict = goDir []
  where
    goDir :: [OsPath] -> OsPath -> IO [OsPath]
    goDir acc root = do
      bracket
        (SafeStreaming.openDirStream root)
        SafeStreaming.closeDirStream
        (goStream root acc)

    goStream :: OsPath -> [OsPath] -> SafeStreaming.DirStream -> IO [OsPath]
    goStream root accum stream = go accum
      where
        go acc = do
          mfn <- SafeStreaming.readDirStream stream
          case mfn of
            Nothing -> do
              SafeStreaming.closeDirStream stream
              pure acc
            Just path -> do
              let absPath = root </> path
              ty <- Streaming.getFileType absPath
              case ty of
                Streaming.Directory -> do
                  acc' <- goDir acc absPath
                  go acc'
                Streaming.DirectorySym -> do
                  acc' <- goDir acc absPath
                  go acc'
                _ ->
                  go (absPath : acc)

getDirectoryContentsRecursiveTar :: OsPath -> IO [OsPath]
getDirectoryContentsRecursiveTar base = recurseDirectories [mempty]
  where
    recurseDirectories :: [OsPath] -> IO [OsPath]
    recurseDirectories [] = pure []
    recurseDirectories (path : paths) = do
      stream <- Streaming.openDirStream (base </> path)
      recurseStream path stream paths
    recurseStream :: OsPath -> Streaming.DirStream -> [OsPath] -> IO [OsPath]
    recurseStream currPath currStream rest = go
      where
        go = unsafeInterleaveIO $ do
          mfn <- Streaming.readDirStream currStream
          case mfn of
            Nothing -> do
              Streaming.closeDirStream currStream
              recurseDirectories rest
            Just fn -> do
              ty <- Streaming.getFileType basePathFn
              case ty of
                Streaming.Directory ->
                  -- (addTrailingPathSeparator pathFn :) <$>
                    recurseStream currPath currStream (pathFn : rest)
                Streaming.DirectorySym ->
                  -- (addTrailingPathSeparator pathFn :) <$>
                    recurseStream currPath currStream (pathFn : rest)
                _ -> (pathFn :) <$> go
              where
                pathFn = currPath </> fn
                basePathFn = base </> pathFn

baselineName :: String
baselineName = "baseline strict"

main :: IO ()
main = do
  -- contents <- getDirectoryContentsRecursive [osp|/tmp/|]
  -- print $ length contents
  let mainDir = [osp|/tmp/file-test-polygon/|]
      dirs    = [mainDir]
  -- let root = [osp|/tmp/file-test-polygon|]
  -- dirs <- fmap (map (root </>)) $ listDirectory root
  print dirs

  let tests =
        [ ( baselineName
          , getDirectoryContentsRecursiveStrict
          )
        , ( "tar package"
          , getDirectoryContentsRecursiveTar
          )
        , ( "System.Directory.OsPath.Contents.getDirectoryContentsRecursive"
          , getDirectoryContentsRecursive
          )
        ]

  counts <- for tests $ \(name, f) -> do
    count <- length <$> f mainDir
    pure (name, count)

  unless (1 == L.length (L.group (L.nubBy ((==) `on` snd) counts))) $
    die $ "Different results:\n" ++ show counts

  defaultMain
    [ mapLeafBenchmarks (addCompare baselineName) $ bgroup (show d)
      [ bench name $ nfAppIO f d
      | (name, f) <- tests
      ]
    | d <- dirs
    ]

addCompare :: String -> [String] -> Benchmark -> Benchmark
addCompare targetBenchName (name : path)
  | name /= targetBenchName
  = bcompare (printAwkExpr (locateBenchmark (targetBenchName : path)))
addCompare _ _ = id
