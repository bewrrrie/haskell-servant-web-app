module Main where

import App
import Data.ByteString as B
import GHC.Word (Word16)
import Hasql.Connection (release)
import Options.Applicative
import SubmissionsDB (connectToDB, initDB)

data Options =
  Options
    { port :: String
    , urlDB :: B.ByteString
    , portDB :: Word16
    , userDB :: B.ByteString
    , passDB :: B.ByteString
    }

main :: IO ()
main = do
  Options { port = _port
          , urlDB = _urlDB
          , portDB = _portDB
          , userDB = _userDB
          , passDB = _passDB
          } <- execParser parserInfo
  connection <- connectToDB _urlDB _portDB _userDB _passDB
  initDB connection
  run connection $ read _port
  release connection

parser :: Parser Options
parser =
  Options <$> portOpt <*> urlDbOpt <*> portDbOpt <*> userDbOpt <*> passDbOpt
  where
    portOpt = argument str (metavar "PORT")
    urlDbOpt = argument str (metavar "URL_DB")
    portDbOpt = argument auto (metavar "PORT_DB")
    userDbOpt = argument str (metavar "USER_DB")
    passDbOpt = argument str (metavar "PASS_DB")

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <>
     progDesc "Run spell check web service" <> header "Sample web app")
