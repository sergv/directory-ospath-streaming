-- |
-- Module:     System.Directory.OsPath.FileType
-- Copyright:  (c) Sergey Vinokurov 2023
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.FileType
  ( FileType(..)
  , getFileType
  ) where

import System.OsPath.Types (OsPath)

#ifdef mingw32_HOST_OS

import System.Directory.OsPath (doesFileExist, doesDirectoryExist)

getFileType :: OsPath -> IO FileType
getFileType fp = do
  isFile <- doesFileExist fp
  if isFile
  then pure File
  else do
    isDir <- doesDirectoryExist fp
    pure $ if isDir then Directory else Other

#endif

#ifndef mingw32_HOST_OS

import Control.Exception (try, IOException)
import System.OsString.Internal.Types (getOsString)
import qualified System.Posix.Files.PosixString as PosixF

getFileType :: OsPath -> IO FileType
getFileType fp = do
  s <- PosixF.getSymbolicLinkStatus $ getOsString fp
  case () of
    _ | PosixF.isRegularFile s  -> pure File
      | PosixF.isDirectory s    -> pure Directory
      | PosixF.isSymbolicLink s -> do
        es' <- try $ PosixF.getFileStatus $ getOsString fp
        case es' of
          Left (_ :: IOException) -> pure Other
          Right s'
            | PosixF.isRegularFile s' -> pure FileSym
            | PosixF.isDirectory s'   -> pure DirectorySym
            | otherwise               -> pure Other
      | otherwise -> pure Other

#endif

data FileType
  = File
  | FileSym -- ^ Symlink to a file.
  | Directory
  | DirectorySym -- ^ Symlink to a directory.
  | Other
  deriving (Show, Read, Eq, Ord)
