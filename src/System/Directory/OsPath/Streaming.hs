{-# LANGUAGE CPP                 #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming functions for interacting with the filesystem.

module System.Directory.OsPath.Streaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream
  ) where

import System.OsPath (osp)

#ifdef mingw32_HOST_OS

import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (unless)
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.OsString.Windows (pstr)
import qualified System.Win32.Types as Win32
import qualified System.Win32.WindowsString.File as Win32

data DirStream = DirStream !Win32.HANDLE !Win32.FindData !Counter

openDirStream :: OsPath -> IO DirStream
openDirStream fp = do
  (h, fdat) <- Win32.findFirstFile $ getOsString fp <> [pstr|\*|]
  hasMore <- Counter.new 1 -- always at least two records, "." and ".."
  pure $! DirStream h fdat hasMore

closeDirStream :: DirStream -> IO ()
closeDirStream (DirStream h _ _) = Win32.findClose h

readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream (DirStream h fdat hasMore) = go
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

-- Don’t use #else to make treesitter do better job - it parses #else part as comments.
#ifndef mingw32_HOST_OS

import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.Posix.Directory.PosixPath (DirStream, closeDirStream)
import qualified System.Posix.Directory.PosixPath as Posix

openDirStream :: OsPath -> IO DirStream
openDirStream = Posix.openDirStream . getOsString

{-# INLINE readDirStream #-}
readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream ds = go
  where
    go = do
      fp <- Posix.readDirStream ds
      case () of
        _ | fp == mempty
          -> pure Nothing
          | fp == getOsString [osp|.|] || fp == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp

#endif
