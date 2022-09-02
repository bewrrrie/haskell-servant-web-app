{-# LANGUAGE OverloadedStrings #-}

module Main where

import Grading

main :: IO ()
main = grade txt >>= print
  where
    txt = "Съешь ещё этих мяхких французских булок и выпей чАя."
