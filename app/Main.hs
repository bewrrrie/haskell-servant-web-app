module Main where

import App
import Data.Functor ((<&>))
import System.Environment

main :: IO ()
main = port >>= run
  where
    port :: IO Int
    port = getArgs <&> read . head
