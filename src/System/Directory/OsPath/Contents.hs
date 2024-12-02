-- |
-- Module:     System.Directory.OsPath.Contents
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.Contents
  ( Rel(..)
  , getDirectoryContentsRecursive

  , listContentsRecFold
  ) where

import Control.Exception (mask, onException)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath

import qualified System.Directory.OsPath.FileType as Streaming
import System.Directory.OsPath.SafeStreaming
import qualified System.Directory.OsPath.SafeStreaming as Streaming

-- | Relative filename, without directory separators
newtype Rel a = Rel { unRel :: a }

-- | List all the files in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is fully consumed.
--
-- Symlinks within directory structure may cause result to be infinitely long.
getDirectoryContentsRecursive
  :: OsPath
  -> IO [OsPath]
getDirectoryContentsRecursive root =
  listContentsRecFold
    Nothing
    (\_ _ _ subdir rest -> subdir rest)
    (\absPath _ _ -> pure (Just absPath))
    (Just root)

{-# INLINE listContentsRecFold #-}
-- | General form of gathering directory contents.
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
      mask $ \restore -> do
        stream <- Streaming.openDirStream dir
        onException
          (restore
            (goDirStream d dir (Streaming.closeDirStream stream *> rest) stream))
          (Streaming.closeDirStream stream)

    goDirStream :: Int -> OsPath -> IO [a] -> DirStream -> IO [a]
    goDirStream !d dir rest stream = go d
      where
        go :: Int -> IO [a]
        go 0     = rest
        go depth = do
          x <- Streaming.readDirStream stream
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
