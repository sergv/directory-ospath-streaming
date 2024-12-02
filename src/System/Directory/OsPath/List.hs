-- |
-- Module:     System.Directory.OsPath.Contents
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.Contents
  ( Rel(..)
  , listContentsRecFold
  , getDirectoryContentsRecursive
  ) where

import Control.Exception (bracket)
import System.Directory.OsPath.Streaming
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath

import qualified System.Directory.OsPath.FileType as Streaming
import qualified System.Directory.OsPath.Streaming as Streaming

-- -- | Absolute filename
-- newtype Abs a = Abs { unAbs :: a }

-- | Relative filename, without directory separators
newtype Rel a = Rel { unRel :: a }

getDirectoryContentsRecursive
  :: OsPath
  -> IO [OsPath]
getDirectoryContentsRecursive root = do
  putStrLn "getDirectoryContentsRecursive: started"
  paths <- listContentsRecFold
    Nothing
    (\_ _ _ subdir rest -> subdir rest)
    (\absPath _ _ -> pure (Just absPath))
    (Just root)
  putStrLn "getDirectoryContentsRecursive: got paths"
  for_ paths $ \path -> do
    putStrLn $ "getDirectoryContentsRecursive: got " ++ show path


-- | Most general file searching.
--
-- Treats symlinks the same as regular files and directories. Folding functions can
-- decide how to handle symlinks.
listContentsRecFold
  :: forall f a. Foldable f
  => Maybe Int
  -- ^ Depth limit if specified, negative values treated the same as positive ones.
  -> (OsPath -> Rel OsPath -> Streaming.FileType -> (IO [a] -> IO [a]) -> IO [a] -> IO [a])
  -- ^ Fold directory by running passed IO action that will scan its contents.
  -- Can ignore the action to avoid traversing the directory.
  -> (OsPath -> Rel OsPath -> Streaming.FileType -> IO (Maybe a))
  -- ^ What to do with file
  -> f OsPath
  -- ^ Roots to search in, either absolute or relative
  -> IO [a]
listContentsRecFold depthLimit foldDir filePred =
  foldr (goNewDir initLimit) (pure [])
  where
    !initLimit = case depthLimit of
      Nothing -> (- 1) -- Loop until overflow, basically infinitely
      Just x  -> abs x

    goNewDir :: Int -> OsPath -> IO [a] -> IO [a]
    goNewDir !d dir rest = do
      putStrLn $ "dir = " ++ show dir
      bracket
        (Streaming.openDirStream dir)
        Streaming.closeDirStream
        (\stream -> goDirStream d dir (Streaming.closeDirStream stream *> rest) stream)

    goDirStream :: Int -> OsPath -> IO [a] -> DirStream -> IO [a]
    goDirStream !d dir rest stream = go d
      where
        go :: Int -> IO [a]
        go 0     = rest
        go depth = do
          x <- Streaming.readDirStream stream
          putStrLn $ show x
          case x of
            Nothing -> rest
            Just y  -> do
              let y' :: OsPath
                  y' = dir </> y
              ft <- Streaming.getFileType y'
              case ft of
                Streaming.Other        -> addLazy (filePred y' (Rel y) ft) (go depth)
                Streaming.File         -> addLazy (filePred y' (Rel y) ft) (go depth)
                Streaming.FileSym      -> addLazy (filePred y' (Rel y) ft) (go depth)
                Streaming.Directory    -> foldDir y' (Rel y) ft (goNewDir (depth - 1) y') (go depth)
                Streaming.DirectorySym -> foldDir y' (Rel y) ft (goNewDir (depth - 1) y') (go depth)

    addLazy :: IO (Maybe a) -> IO [a] -> IO [a]
    addLazy x y = do
      x' <- x
      case x' of
        Nothing  -> y
        Just x'' -> (x'' :) <$> unsafeInterleaveIO y
