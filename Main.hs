#!/usr/bin/env stack
{- stack
   --resolver lts-12.0
   --install-ghc
   script
   --ghc-options -Werror
   --ghc-options -Wall
   --
-}

{-# LANGUAGE Trustworthy #-}

module Main (main) where

import           System.IO.Unsafe
  ( unsafePerformIO
  )

import           Data.Batcher
  ( SortableTrust
  , sort
  )

trust :: SortableTrust a
trust = unsafePerformIO . sort

main :: IO ()
main =
  do
    putStrLn $ "# Batcher sort"
    putStrLn $ "> xs (initial): " ++ (show  xs)
    sort xs >>= putStrLn . ("> ys (effects): " ++) . show
    putStrLn $ "> zs (trusted): " ++ (show $ trust xs)
      where
        xs = reverse [ 0 .. 15 ] :: [ Word ]