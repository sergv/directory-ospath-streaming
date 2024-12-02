-- |
-- Module:     System.Directory.OsPath.SafeStreaming
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Directory.OsPath.SafeStreaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream
  ) where

import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (when)
import System.OsPath (OsPath)

import qualified System.Directory.OsPath.Streaming as Streaming

-- | Abstract handle to directory contents. Safe to close multiple times.
data DirStream = DirStream
  { dsHandle   :: !Streaming.DirStream
  , dsIsClosed :: !Counter
  }

openDirStream :: OsPath -> IO DirStream
openDirStream path = do
  dsHandle   <- Streaming.openDirStream path
  dsIsClosed <- Counter.new 0
  pure DirStream{dsHandle, dsIsClosed}

-- | Deallocate directory handle. It’s safe to close 'DirStream' multiple times,
-- unlike the underlying OS-specific directory stream handle.
closeDirStream :: DirStream -> IO ()
closeDirStream DirStream{dsHandle, dsIsClosed} = do
  !oldVal <- Counter.cas dsIsClosed 0 1
  when (oldVal == 0) $
    Streaming.closeDirStream dsHandle

readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream = Streaming.readDirStream . dsHandle
