-- |
-- Module:     System.Directory.OsPath.Streaming.Internal.Raw
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnboxedTuples       #-}

-- | Streaming functions for interacting with the filesystem.
--
-- These do the basic job of reading directory entries but care must
-- be taken to not close these streams more than once.

module System.Directory.OsPath.Streaming.Internal.Raw
  ( DirStream(..)
  , openDirStream
  , readDirStream
  , closeDirStream

  , DirReadCache(..)
  , allocateDirReadCache
  , releaseDirReadCache
  , readDirStreamWithCache
  ) where

import Control.Monad ((<=<))
import Data.Coerce (coerce)
import System.OsPath (osp, (</>))

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

-- Don’t use #else to make treesitter do better job - it parses #else part as comments.
#ifndef mingw32_HOST_OS
import System.OsPath.Types (OsPath)
import System.OsString.Internal.Types (OsString(OsString), getOsString)
import qualified System.Posix.Directory.PosixPath as Posix

# if MIN_VERSION_unix(2, 8, 6)
import Foreign.C (CString, CChar)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (sizeOf, alignment, peekElemOff)
import qualified System.Posix.Directory.Internals as DirInternals
import System.Posix.PosixPath.FilePath (peekFilePath)

import GHC.Prim (MutableByteArray#, newAlignedPinnedByteArray#, touch#, mutableByteArrayContents#, RealWorld)
import GHC.Ptr (Ptr(..))
import GHC.Types (IO(..), Int(..))
# endif
#endif

-- | Abstract handle to directory contents.
--
-- Not thread safe and shouldn't be closed more than once.

#ifdef mingw32_HOST_OS
data DirStream = DirStream !Win32.HANDLE !Win32.FindData !Counter
#endif
#ifndef mingw32_HOST_OS
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
-- on the same handle.
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
    !(I# size)  = sizeOf    (undefined :: Ptr DirInternals.DirEnt)
    !(I# align) = alignment (undefined :: Ptr DirInternals.DirEnt)
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
releaseDirReadCache (DirReadCache barr#) =
  IO $ \s0 -> case touch# barr# s0 of s1 -> (# s1, () #)
# endif
#endif


readDirStreamWithCache
  :: DirReadCache
  -> OsPath -- ^ Root that DirStream belongs to
  -> DirStream
  -> IO (Maybe (OsPath, Basename OsPath, FileType))
#ifdef mingw32_HOST_OS
readDirStreamWithCache _ dir = do
  traverse (\x -> let full = dir </> x in (full, Basename x,) <$> getFileType full) <=< readDirStream
#endif
#ifndef mingw32_HOST_OS

# if !MIN_VERSION_unix(2, 8, 6)
readDirStreamWithCache _ dir = do
  traverse (\x -> let full = dir </> x in (full, Basename x,) <$> getFileType full) <=< readDirStream
# endif

# if MIN_VERSION_unix(2, 8, 6)
readDirStreamWithCache (DirReadCache barr#) root (DirStream stream) = go
  where
    cache :: Ptr DirInternals.DirEnt
    cache = Ptr (mutableByteArrayContents# barr#)

    shouldSkipDirEntry :: CString -> IO Bool
    shouldSkipDirEntry ptr
      | ptr == nullPtr = pure True
    shouldSkipDirEntry ptr = do
      (x1 :: CChar) <- peekElemOff ptr 0
      case x1 of
        0  -> pure False
        46 -> do -- ASCII for ‘.’
          (x2 :: CChar) <- peekElemOff ptr 1
          case x2 of
            0  -> pure True
            46 -> do -- ASCII for ‘.’
              (x3 :: CChar) <- peekElemOff ptr 2
              pure $! x3 == 0
            _  -> pure False
        _  -> pure False

    go :: IO (Maybe (OsPath, Basename OsPath, FileType))
    go = do
      x <- DirInternals.readDirStreamWithPtr
        cache
        (\dirEnt -> do
          (namePtr :: CString) <- DirInternals.dirEntName dirEnt

          shouldSkip <- shouldSkipDirEntry namePtr

          if shouldSkip
          then
            pure Nothing
          else do
            !path <- peekFilePath namePtr

            let fullPath = root </> coerce path

            !typ  <- DirInternals.dirEntType dirEnt

            typ' <- case typ of
              DirInternals.UnknownType         -> getFileType fullPath
              DirInternals.NamedPipeType       -> pure Other
              DirInternals.CharacterDeviceType -> pure Other
              DirInternals.DirectoryType       -> pure regularDirectory
              DirInternals.BlockDeviceType     -> pure Other
              DirInternals.RegularFileType     -> pure regularFile
              DirInternals.SymbolicLinkType    -> getFileType fullPath
              DirInternals.SocketType          -> pure Other
              DirInternals.WhiteoutType        -> pure Other
              -- Unaccounted type, probably should not happeen since the
              -- list above is exhaustive.
              _                                -> getFileType fullPath

            pure (Just (fullPath, Basename $ coerce path, typ')))
        stream

      case x of
        Nothing           -> pure Nothing
        Just Nothing      -> go
        Just res@(Just _) -> pure res
# endif
#endif
