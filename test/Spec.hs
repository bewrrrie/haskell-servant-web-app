module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Grading.hs", "src/Spelling.hs"]
