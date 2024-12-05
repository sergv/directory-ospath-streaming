-- |
-- Module:     System.Directory.OsPath.Contents
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.Contents
  ( getDirectoryContentsRecursive

  , listContentsRecFold
  ) where

import Control.Exception (mask, onException)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath

import qualified System.Directory.OsPath.FileType as Streaming
import System.Directory.OsPath.SafeStreaming
import qualified System.Directory.OsPath.SafeStreaming as Streaming
import System.Directory.OsPath.Types

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
    (\_ _ _ prependSubdir -> pure $ \rest -> prependSubdir rest)
    (\absPath _ _ -> pure (Just absPath))
    (Just root)
Endo

{-# INLINE listContentsRecFold #-}
-- | The most general form of gathering directory contents (so far).
--
-- Treats symlinks the same as regular files and directories. Folding functions can
-- decide how to handle symlinks.
--
-- Both directory and file actions can throw exceptions and this function
-- will try extra hard not to leak opened directory stream resources (you will
-- spend some effort too thanks to interesting type signatures that make this possible).
listContentsRecFold
  :: forall f a. Foldable f
  => Maybe Int
  -- ^ Depth limit if specified, negative values treated the same as positive ones.
  -> (OsPath -> Basename OsPath -> Streaming.FileType -> (IO [a] -> IO [a]) -> IO (IO [a] -> IO [a]))
  -- ^ Prepare to fold directory given its path.
  --
  -- Can do IO actions to plan what to do and must produce function of
  -- type @IO [a] -> IO [a]@ that will receive IO action analyzing the
  -- rest of the filesystem (@IO [a]@) and return final result (i.e. result from
  -- analyzing the rest plus results for this directory on top).
  --
  -- The passed @(IO [a] -> IO [a])@ argument function should (but is not required to)
  -- be applied in the returned function and it will prepend results for subdirectories
  -- of the directory being analyzed. If not applied thes subdirectories will be skipped,
  -- this way ignoring particular directory and all its children can be achieved.
  -> (OsPath -> Basename OsPath -> Streaming.FileType -> IO (Maybe a))
  -- ^ What to do with file
  -> f OsPath
  -- ^ Roots to search in, either absolute or relative
  -> IO [a]
listContentsRecFold depthLimit foldDir filePred input =
  listContentsRecFold' =<< Streaming.allocateDirReadCache
  where
    listContentsRecFold' cache =
      foldr (goNewDir initLimit) (Streaming.releaseDirReadCache cache *> pure []) input
      where
        !initLimit = case depthLimit of
          Nothing -> -1 -- Loop until overflow, basically infinitely
          Just x  -> abs x

        goNewDir :: Int -> OsPath -> IO [a] -> IO [a]
        goNewDir !d dir rest = do
          mask $ \restore -> do
            stream <- Streaming.openDirStream dir
            (restore
              (goDirStream d (Streaming.closeDirStream stream *> rest) stream))

        goDirStream :: Int -> IO [a] -> DirStream -> IO [a]
        goDirStream 0     rest _      = rest
        goDirStream depth rest stream = go
          where
            go :: IO [a]
            go = do
              x <- Streaming.readDirStreamWithCache cache stream `onException`
                Streaming.closeDirStream stream
              case x of
                Nothing          -> rest
                Just (y', y, ft) -> do
                  case ft of
                    Streaming.Other        -> addLazy (filePred y' y ft) go
                    Streaming.File         -> addLazy (filePred y' y ft) go
                    Streaming.FileSym      -> addLazy (filePred y' y ft) go
                    Streaming.Directory    -> do
                      k <- foldDir y' y ft (goNewDir (depth - 1) y') `onException`
                        Streaming.closeDirStream stream
                      k go
                    Streaming.DirectorySym -> do
                      k <- foldDir y' y ft (goNewDir (depth - 1) y') `onException`
                        Streaming.closeDirStream stream
                      k go

            addLazy :: IO (Maybe a) -> IO [a] -> IO [a]
            addLazy x y = do
              x' <- x `onException` Streaming.closeDirStream stream
              case x' of
                Nothing  -> y
                Just x'' -> (x'' :) <$> unsafeInterleaveIO y
