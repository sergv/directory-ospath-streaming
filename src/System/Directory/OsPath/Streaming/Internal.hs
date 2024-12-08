-- |
-- Module:     System.Directory.OsPath.Streaming.Internal
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo    #-}
{-# LANGUAGE UnboxedTuples  #-}

module System.Directory.OsPath.Streaming.Internal
  ( DirStream(..)
  , openDirStream
  , readDirStream
  , closeDirStream

  , readDirStreamWithCache
  ) where

import Control.Concurrent.Counter (Counter)
import qualified Control.Concurrent.Counter as Counter
import Control.Monad (when)
import System.Mem.Weak (Weak, mkWeak, finalize)
import System.OsPath (OsPath)

import qualified System.Directory.OsPath.Streaming.Internal.Raw as Raw
import System.Directory.OsPath.Types

-- | Abstract handle to directory contents. Safe to close multiple times.
data DirStream = DirStream
  { dsHandle   :: {-# UNPACK #-} !Raw.DirStream
  , dsIsClosed :: {-# UNPACK #-} !Counter
  , dsPath     :: OsPath
  , dsFin      :: {-# UNPACK #-} !(Weak DirStream)
  }

openDirStream :: OsPath -> IO DirStream
openDirStream dsPath = mdo
  dsHandle   <- Raw.openDirStream dsPath
  dsIsClosed <- Counter.new 0
  let result = DirStream{dsHandle, dsIsClosed, dsPath, dsFin}
  dsFin <- mkWeak result result (Just (closeDirStreamInternal result))
  pure result

-- | Deallocate directory handle. It’s safe to close 'DirStream' multiple times,
-- unlike the underlying OS-specific directory stream handle.
closeDirStream :: DirStream -> IO ()
closeDirStream stream =
  -- Finalize ourselves to do it only once instead of running finalizer
  -- in GC afterwards once more.
  finalize (dsFin stream)

closeDirStreamInternal :: DirStream -> IO ()
closeDirStreamInternal DirStream{dsHandle, dsIsClosed} = do
  !oldVal <- Counter.cas dsIsClosed 0 1
  when (oldVal == 0) $
    Raw.closeDirStream dsHandle

readDirStream :: DirStream -> IO (Maybe OsPath)
readDirStream = Raw.readDirStream . dsHandle

readDirStreamWithCache
  :: Raw.DirReadCache
  -> DirStream
  -> IO (Maybe (OsPath, Basename OsPath, FileType))
readDirStreamWithCache cache DirStream{dsHandle, dsPath} =
  Raw.readDirStreamWithCache cache dsPath dsHandle
