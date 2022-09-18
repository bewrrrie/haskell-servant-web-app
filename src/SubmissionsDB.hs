{-# LANGUAGE OverloadedStrings #-}

module SubmissionsDB
  ( connectToDB
  , initDB
  , runSession
  , addSubmission
  , ConnInfo(..)
  ) where

import Contravariant.Extras.Contrazip (contrazip4)
import Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Time.LocalTime (LocalTime, getZonedTime, zonedTimeToLocalTime)
import GHC.Int (Int16)
import GHC.Word (Word16)
import Hasql.Connection (settings)
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Pool (Pool, acquire, use)
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement(Statement))

data ConnInfo =
  ConnInfo
    { poolSize :: !Int
    , timeout :: !Int
    , host :: !C.ByteString
    , port :: !Word16
    , user :: !C.ByteString
    , pass :: !C.ByteString
    }

connectToDB :: ConnInfo -> IO Pool
connectToDB (ConnInfo _poolSize _timeout _host _port _user _pass) =
  acquire (_poolSize, toEnum $ toSeconds _timeout, connSettings)
  where
    toSeconds = (* 1000000000000)
    connSettings = settings _host _port _user _pass ""

createSubbmissionsTableStatement :: Statement () ()
createSubbmissionsTableStatement =
  let sql =
        "create table if not exists submissions ( \
        \ id serial not null primary key, \
        \ datetime timestamp not null, \
        \ name text not null, \
        \ text text not null, \
        \ score int not null )"
      encoder = E.noParams
      decoder = D.noResult
   in Statement sql encoder decoder True

initDB :: Pool -> IO ()
initDB pool = runSession pool $ statement () createSubbmissionsTableStatement

runSession :: Pool -> Session a -> IO a
runSession pool session = do
  sessionResult <- use pool session
  case sessionResult of
    Right result -> return result
    Left err -> error $ show err

insertSubmissionStatement :: Statement (LocalTime, T.Text, T.Text, Int16) ()
insertSubmissionStatement =
  let sql =
        "insert into submissions (datetime, name, text, score) \
        \values ($1, $2, $3, $4)"
      encoder =
        contrazip4
          (E.param (E.nonNullable E.timestamp))
          (E.param (E.nonNullable E.text))
          (E.param (E.nonNullable E.text))
          (E.param (E.nonNullable E.int2))
      decoder = D.noResult
   in Statement sql encoder decoder True

addSubmission :: Pool -> T.Text -> T.Text -> Int16 -> IO ()
addSubmission pool name txt score = do
  zonedTimestamp <- getZonedTime
  let localTimestamp = zonedTimeToLocalTime zonedTimestamp
  runSession pool $
    statement (localTimestamp, name, txt, score) insertSubmissionStatement
