{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Streaming functions for interacting with the filesystem.

module System.Directory.OsPath.Streaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream

  , DirReadCache
  , allocateDirReadCache
  , releaseDirReadCache
  , readDirStreamWithCache
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

# if MIN_VERSION_unix(2, 8, 6)
import Control.Monad ((>=>))
import Foreign.Ptr (Ptr)
import Foreign.Storable (sizeOf, alignment)
import System.Posix.Directory.Internals (DirEnt, readDirStreamWithPtr, dirEntName)
import System.Posix.PosixPath.FilePath (peekFilePath)

import GHC.Prim (MutableByteArray#, newAlignedPinnedByteArray#, touch#, mutableByteArrayContents#, RealWorld)
import GHC.Ptr (Ptr(..))
import GHC.Types (IO(..), Int(..))
# endif
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


#ifdef mingw32_HOST_OS
-- No state on Windows
newtype DirReadCache = DirReadCache ()
#endif

#ifndef mingw32_HOST_OS

# if !MIN_VERSION_unix(2, 8, 6)
-- No state in early unix package
newtype DirReadCache = DirReadCache ()
# endif

# if MIN_VERSION_unix(2, 8, 6)
data DirReadCache = DirReadCache (MutableByteArray# RealWorld)
# endif

#endif


allocateDirReadCache :: IO DirReadCache

#ifdef mingw32_HOST_OS
allocateDirReadCache = pure $ DirReadCache ()
#endif

#ifndef mingw32_HOST_OS

# if !MIN_VERSION_unix(2, 8, 6)
allocateDirReadCache = pure $ DirReadCache ()
# endif

# if MIN_VERSION_unix(2, 8, 6)
allocateDirReadCache = IO $ \s0 ->
   case newAlignedPinnedByteArray# size align s0 of
     (# s1, mbarr# #) ->
       (# s1, DirReadCache mbarr# #)
  where
    !(I# size)  = sizeOf    (undefined :: Ptr DirEnt)
    !(I# align) = alignment (undefined :: Ptr DirEnt)
# endif

#endif

releaseDirReadCache :: DirReadCache -> IO ()

#ifdef mingw32_HOST_OS
releaseDirReadCache _ = pure ()
#endif

#ifndef mingw32_HOST_OS

# if !MIN_VERSION_unix(2, 8, 6)
releaseDirReadCache _ = pure ()
# endif

# if MIN_VERSION_unix(2, 8, 6)
releaseDirReadCache (DirReadCache barr#) = IO $ \s0 -> case touch# barr# s0 of
  s1 -> (# s1, () #)
# endif

#endif


readDirStreamWithCache :: DirReadCache -> DirStream -> IO (Maybe OsPath)
#ifdef mingw32_HOST_OS
readDirStreamWithCache _ = readDirStream
#endif

#ifndef mingw32_HOST_OS

# if !MIN_VERSION_unix(2, 8, 6)
readDirStreamWithCache _ = readDirStream
# endif

# if MIN_VERSION_unix(2, 8, 6)
readDirStreamWithCache (DirReadCache barr#) (DirStream stream) = go
  where
    cache :: Ptr DirEnt
    cache = Ptr (mutableByteArrayContents# barr#)
    go = do
      fp <- readDirStreamWithPtr cache (dirEntName >=> peekFilePath) stream
      case fp of
        Nothing -> pure Nothing
        Just fp'
          | fp' == getOsString [osp|.|] || fp' == getOsString [osp|..|]
            -> go
          | otherwise
            -> pure $ Just $ OsString fp'
  -- readDirStream stream
# endif

#endif
