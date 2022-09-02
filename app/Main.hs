{-# LANGUAGE OverloadedStrings #-}

module Main where

import Speller

main :: IO ()
main = do
  res <- spell "синхрафазатрон+в+дубне"
  print res
