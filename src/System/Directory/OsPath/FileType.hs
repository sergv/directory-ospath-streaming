-- |
-- Module:     System.Directory.OsPath.FileType
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.FileType
  ( getFileType

  , regularFile
  , regularDirectory
  , symlinkFile
  , symlinkDirectory
  ) where

import System.OsPath.Types (OsPath)
import System.Directory.OsPath.Types

#ifdef mingw32_HOST_OS
import System.Directory.OsPath (doesFileExist, doesDirectoryExist)
#endif
#ifndef mingw32_HOST_OS
import Control.Exception (try, IOException)
import System.OsString.Internal.Types (getOsString)
import qualified System.Posix.Files.PosixString as PosixF
#endif

getFileType :: OsPath -> IO FileType
#ifdef mingw32_HOST_OS
getFileType fp = do
  isFile <- doesFileExist fp
  if isFile
  then pure regularFile
  else do
    isDir <- doesDirectoryExist fp
    pure $ if isDir then regularDirectory else Other
#endif
#ifndef mingw32_HOST_OS
getFileType fp = do
  s <- PosixF.getSymbolicLinkStatus $ getOsString fp
  case () of
    _ | PosixF.isRegularFile s  -> pure regularFile
      | PosixF.isDirectory s    -> pure regularDirectory
      | PosixF.isSymbolicLink s -> do
        es' <- try $ PosixF.getFileStatus $ getOsString fp
        case es' of
          Left (_ :: IOException) -> pure Other
          Right s'
            | PosixF.isRegularFile s' -> pure symlinkFile
            | PosixF.isDirectory s'   -> pure symlinkDirectory
            | otherwise               -> pure Other
      | otherwise -> pure Other
#endif

-- Avoid allocations with this one weird trick.
{-# NOINLINE regularFile #-}
{-# NOINLINE regularDirectory #-}
{-# NOINLINE symlinkFile #-}
{-# NOINLINE symlinkDirectory #-}
-- | Auxiliary constants to refer to different file types without
-- allocations.
regularFile, regularDirectory, symlinkFile, symlinkDirectory :: FileType
regularFile      = File Regular
regularDirectory = Directory Regular
symlinkFile      = File Symlink
symlinkDirectory = Directory Symlink
