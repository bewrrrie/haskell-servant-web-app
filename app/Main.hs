module Main where

import App (run)
import Hasql.Pool (release)
import Options.Applicative
  ( Parser
  , ParserInfo
  , (<**>)
  , argument
  , auto
  , execParser
  , fullDesc
  , header
  , helper
  , info
  , metavar
  , progDesc
  , str
  )
import SubmissionsDB (ConnInfo(ConnInfo), connectToDB, initDB)

data Options =
  Options
    { port :: String
    , connInfo :: ConnInfo
    }

main :: IO ()
main = do
  Options {port = _port, connInfo = _connInfo} <- execParser parserInfo
  pool <- connectToDB _connInfo
  initDB pool
  run pool $ read _port
  release pool

parser :: Parser Options
parser =
  Options <$> portOpt <*>
  (ConnInfo <$> poolSizeOpt <*> timeoutOpt <*> hostDbOpt <*> portDbOpt <*>
   userDbOpt <*>
   passDbOpt)
  where
    portOpt = argument str (metavar "PORT")
    poolSizeOpt = argument auto (metavar "POOL_SIZE")
    timeoutOpt = argument auto (metavar "TIMEOUT")
    hostDbOpt = argument str (metavar "URL_DB")
    portDbOpt = argument auto (metavar "PORT_DB")
    userDbOpt = argument str (metavar "USER_DB")
    passDbOpt = argument str (metavar "PASS_DB")

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <>
     progDesc "Run spell check web service" <> header "Sample web app")
