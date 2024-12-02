{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Streaming functions for interacting with the filesystem.

module System.Directory.OsPath.Streaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream
  ) where

import Data.Coerce (coerce)
import System.OsPath (osp)

#ifdef mingw32_HOST_OS
import Control.Monad (unless)
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import System.OsString.Windows (pstr)
import qualified System.Win32.Types as Win32
import qualified System.Win32.WindowsString.File as Win32
#endif

-- Don’t use #else to make treesitter do better job - it parses #else part as comments.
#ifndef mingw32_HOST_OS
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import qualified System.Posix.Directory.PosixPath as Posix
#endif

#ifdef mingw32_HOST_OS
-- | Abstract handle to directory contents.
data DirStream = DirStream !Win32.HANDLE !Win32.FindData !Counter
#endif

#ifndef mingw32_HOST_OS
-- | Abstract handle to directory contents.
newtype DirStream = DirStream Posix.DirStream
#endif

openDirStream :: OsPath -> IO DirStream
#ifdef mingw32_HOST_OS
openDirStream fp = do
  (h, fdat) <- Win32.findFirstFile $ getOsString fp <> [pstr|\*|]
  hasMore <- Counter.new 1 -- always at least two records, "." and ".."
  pure $! DirStream h fdat hasMore
#endif

#ifndef mingw32_HOST_OS
openDirStream =
  coerce . Posix.openDirStream . getOsString
#endif

-- | Deallocate directory handle. It’s not safe to call multiple times
-- | on the same handle.
closeDirStream :: DirStream -> IO ()

#ifdef mingw32_HOST_OS
closeDirStream (DirStream h _ _) = Win32.findClose h
#endif

#ifndef mingw32_HOST_OS
closeDirStream = coerce Posix.closeDirStream
#endif

readDirStream :: DirStream -> IO (Maybe OsPath)

#ifdef mingw32_HOST_OS
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

#ifndef mingw32_HOST_OS
readDirStream (DirStream stream) = go
  where

# if !MIN_VERSION_unix(2, 8, 6)
    go = do
      fp <- Posix.readDirStream stream
      case () of
        _ | fp == mempty
          -> pure Nothing
          | fp == getOsString [osp|.|] || fp == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp
# endif

# if MIN_VERSION_unix(2, 8, 6)
    go = do
      fp <- Posix.readDirStreamMaybe stream
      case fp of
        Nothing -> pure Nothing
        Just fp'
          | fp' == getOsString [osp|.|] || fp' == getOsString [osp|..|]
          -> go
          | otherwise
          -> pure $ Just $ OsString fp'
# endif

{-# INLINE readDirStream #-}
#endif
