-- |
-- Module:     System.Directory.OsPath.Types
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module System.Directory.OsPath.Types
  ( SymlinkType(..)
  , FileType(..)
  , Basename(..)
  ) where

import GHC.Generics (Generic, Generic1)

data SymlinkType = Regular | Symlink
  deriving (Show, Read, Eq, Ord, Generic)

data FileType
  = File {-# UNPACK #-} !SymlinkType
  | Directory {-# UNPACK #-} !SymlinkType
  | Other
  deriving (Show, Read, Eq, Ord, Generic)

-- | Basename part of filename, without directory separators.
newtype Basename a = Basename { unBasename :: a }
  deriving (Eq, Ord, Show, Generic, Generic1, Functor, Foldable, Traversable)

