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
  ( Basename(..)
  ) where

import GHC.Generics (Generic)

-- | Basename part of  filename, without directory separators.
newtype Basename a = Basename { unBasename :: a }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

