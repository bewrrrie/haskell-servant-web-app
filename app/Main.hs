{-# LANGUAGE OverloadedStrings #-}

module Main where

import App
import qualified Data.ByteString.Char8 as C
import Data.Functor ((<&>))
import Hasql.Connection (release)
import SubmissionsDB (connectToDB, initDB)
import System.Environment

main :: IO ()
main = do
  [port, urlDB, portDB, userDB, passDB] <- getArgs <&> take 5
  let (_urlDb, _portDB, _userDB, _passDB) =
        (C.pack urlDB, read portDB, C.pack userDB, C.pack passDB)
  connection <- connectToDB _urlDb _portDB _userDB _passDB
  initDB connection
  run connection $ read port
  release connection
