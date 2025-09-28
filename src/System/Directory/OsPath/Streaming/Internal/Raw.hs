-- |
-- Module:     System.Directory.OsPath.Streaming.Internal.Raw
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com
--
-- Streaming functions for interacting with the filesystem.
--
-- These do the basic job of reading directory entries but care must
-- be taken to not close these streams more than once.

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}

module System.Directory.OsPath.Streaming.Internal.Raw
  ( RawDirStream(..)
  , openRawDirStream
  , readRawDirStream
  , closeRawDirStream
  ) where

import System.OsPath (osp, addTrailingPathSeparator)

import System.Directory.OsPath.FileType
import System.Directory.OsPath.Types

#ifdef mingw32_HOST_OS
import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (unless)
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.OsString.Windows (pstr)
import qualified System.Win32.Types as Win32
import qualified System.Win32.WindowsString.File as Win32
#endif

-- Don’t use #else to make treesitter do a better job - it parses #else part as comments.
#ifndef mingw32_HOST_OS
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import qualified System.Posix.Directory.PosixPath as Posix
#endif

-- | Abstract handle to directory contents.
--
-- Not thread safe and shouldn't be closed more than once.

#ifdef mingw32_HOST_OS
data RawDirStream = RawDirStream !Win32.HANDLE !Win32.FindData !Counter !OsPath
#endif
#ifndef mingw32_HOST_OS
data RawDirStream = RawDirStream !Posix.DirStream !OsPath
#endif

openRawDirStream :: OsPath -> IO RawDirStream
#ifdef mingw32_HOST_OS
openRawDirStream fp = do
  (h, fdat) <- Win32.findFirstFile $ getOsString fp <> [pstr|\*|]
  hasMore <- Counter.new 1 -- always at least two records, "." and ".."
  pure $! RawDirStream h fdat hasMore $ addTrailingPathSeparator fp
#endif

#ifndef mingw32_HOST_OS
openRawDirStream root = do
  stream <- Posix.openDirStream (getOsString root)
  pure $ RawDirStream stream $ addTrailingPathSeparator root
#endif

-- | Deallocate directory handle. It’s not safe to call multiple times
-- on the same handle.
closeRawDirStream :: RawDirStream -> IO ()

#ifdef mingw32_HOST_OS
closeRawDirStream (RawDirStream h _ _ _) = Win32.findClose h
#endif
#ifndef mingw32_HOST_OS
closeRawDirStream (RawDirStream stream _) = Posix.closeDirStream stream
#endif

readRawDirStream
  :: RawDirStream
  -> IO (Maybe (OsPath, Basename OsPath, FileType))
#ifdef mingw32_HOST_OS
readRawDirStream stream@(RawDirStream _ _ _ root) =
  readRawDirStreamSimple stream >>=
    traverse (\x -> let full = root </> x in (full, Basename x,) <$> getFileType full)
#endif
#ifndef mingw32_HOST_OS
readRawDirStream stream@(RawDirStream _ root) =
  readRawDirStreamSimple stream >>=
    traverse (\x -> let full = root </> x in (full, Basename x,) <$> getFileType full)
#endif

readRawDirStreamSimple :: RawDirStream -> IO (Maybe OsPath)
#ifdef mingw32_HOST_OS
readRawDirStreamSimple (RawDirStream h fdat hasMore _) = go
  where
    go = do
      hasMore' <- Counter.get hasMore
      if hasMore' /= 0
      then do
        filename  <- Win32.getFindDataFileName fdat
        hasMore'' <- Win32.findNextFile h fdat
        unless hasMore'' $
          Counter.set hasMore 0
        if filename == getOsString [osp|.|] || filename == getOsString [osp|..|]
        then go
        else pure $ Just $ OsString filename
      else pure Nothing
#endif
#ifndef mingw32_HOST_OS
readRawDirStreamSimple (RawDirStream stream _) = go
  where
    go = do
      fp <- Posix.readDirStream stream
      case () of
        _ | fp == mempty
          -> pure Nothing
          | fp == getOsString [osp|.|] || fp == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp
#endif

