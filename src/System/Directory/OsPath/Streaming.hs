-- |
-- Module:     System.Directory.OsPath.SafeStreaming
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module System.Directory.OsPath.Streaming
  ( DirStream
  , openDirStream
  , readDirStream
  , closeDirStream

  -- * File types
  , SymlinkType(..)
  , FileType(..)
  , Basename(..)
  , getFileType

  -- * Get directory contents
  , getDirectoryContentsRecursive

  , listContentsRecFold

  -- * Utilities
  , regularFile
  , regularDirectory
  , symlinkFile
  , symlinkDirectory
  ) where

import System.Directory.OsPath.Contents
import System.Directory.OsPath.FileType
import System.Directory.OsPath.Streaming.Internal as Streaming
import System.Directory.OsPath.Types

