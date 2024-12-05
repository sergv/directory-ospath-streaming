-- |
-- Module:     TestLazyIO
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TestLazyIO (main) where

import Control.Exception
import Data.Foldable (for_)
import System.IO.Unsafe (unsafeInterleaveIO)

stream :: forall a. (Enum a, Eq a, Show a) => (a -> Bool) -> a -> a -> IO [a]
stream p start end = go start
  where
    go :: a -> IO [a]
    go !x
      | x == end  = error "the end" -- pure []
      | otherwise = do
        putStrLn $ "Producing " ++ show x
        onException
          ((x :) <$> unsafeInterleaveIO (go (succ x)))
          (putStrLn $ "Cleaning " ++ show x)

main :: IO ()
main = do
  xs <- stream @Int (\x -> x `mod` 5 == 0) 1 100
  for_ xs $ \x ->
    putStrLn $ "Consuming " ++ show x
  pure ()
