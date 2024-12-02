-- |
-- Module:     System.Directory.OsPath.Contents
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.Contents
  ( getDirectoryContentsRecursive

  , listContentsRecFold
  ) where

import Control.Exception (mask, onException)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath

import System.Directory.OsPath.Streaming.Internal (DirStream)
import qualified System.Directory.OsPath.Streaming.Internal as Streaming
import qualified System.Directory.OsPath.Streaming.Internal.Raw as Raw
import System.Directory.OsPath.Types

-- | Recursively list all the files and directories in a directory and all subdirectories.
--
-- The order places files in sub-directories after all the files in their
-- parent directories. The list is generated lazily so is not well defined if
-- the source directory structure changes before the list is fully consumed.
--
-- Symlinks within directory structure may cause result to be infinitely long.
getDirectoryContentsRecursive
  :: OsPath
  -> IO [(OsPath, FileType)]
getDirectoryContentsRecursive root =
  listContentsRecFold
    Nothing
    (\absPath _ ft cons prependSubdir rest -> cons (absPath, ft) $ prependSubdir rest)
    (\absPath _ ft -> pure (Just (absPath, ft)))
    (Just root)

{-# INLINE listContentsRecFold #-}
-- | The most general form of gathering directory contents.
--
-- Treats symlinks the same as regular files and directories. Folding functions can
-- decide how to handle symlinks.
--
-- Both directory and file actions can throw exceptions and this function
-- will try to close finished directory streams promptly (they’ll be closed
-- by GC in the worst case).
listContentsRecFold
  :: forall f a. Foldable f
  => Maybe Int
  -- ^ Depth limit if specified, negative values treated the same as positive ones.
  -> (forall b. OsPath -> Basename OsPath -> FileType -> (a -> IO b -> IO b) -> (IO b -> IO b) -> IO b -> IO b)
  -- ^ Prepare to fold directory given its path.
  --
  -- Can do IO actions to plan what to do and typically should derive its
  -- result from last @IO b@ argument. Ignoring it will terminate content enumeration
  -- and not produce any more results.
  --
  -- The passed @(a -> IO b -> IO b)@ argument function can be used
  -- to record some output about the directory itself.
  --
  -- The passed @(IO b -> IO b)@ argument function should (but is not required to)
  -- be applied in the returned function and it will prepend results for subdirectories
  -- of the directory being analyzed. If not applied thes subdirectories will be skipped,
  -- this way ignoring particular directory and all its children can be achieved.
  -> (OsPath -> Basename OsPath -> FileType -> IO (Maybe a))
  -- ^ What to do with file
  -> f OsPath
  -- ^ Roots to search in, either absolute or relative
  -> IO [a]
listContentsRecFold depthLimit foldDir filePred input =
  listContentsRecFold' =<< Raw.allocateDirReadCache
  where
    listContentsRecFold' cache =
      foldr (goNewDir initLimit) (Raw.releaseDirReadCache cache *> pure []) input
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
            go = (`onException` Streaming.closeDirStream stream) $ do
              x <- Streaming.readDirStreamWithCache cache stream
              case x of
                Nothing          -> rest
                Just (y', y, ft) -> do
                  case ft of
                    Other       -> addLazy (filePred y' y ft) go
                    File _      -> addLazy (filePred y' y ft) go
                    Directory _ -> foldDir y' y ft cons (goNewDir (depth - 1) y') go

            addLazy :: IO (Maybe a) -> IO [a] -> IO [a]
            addLazy x y = do
              x' <- x
              case x' of
                Nothing  -> y
                Just x'' -> cons x'' y

            cons :: a -> IO [a] -> IO [a]
            cons x y =
              (x :) <$> unsafeInterleaveIO y
