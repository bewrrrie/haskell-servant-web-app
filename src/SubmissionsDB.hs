{-# LANGUAGE OverloadedStrings #-}

module SubmissionsDB
  ( connectToDB
  , initDB
  , runSession
  , addSubmission
  ) where

import Contravariant.Extras.Contrazip
import Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Time.LocalTime
import GHC.Int
import GHC.Word
import Hasql.Connection
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import Hasql.Session (Session, run, statement)
import Hasql.Statement

connectToDB ::
     C.ByteString -> Word16 -> C.ByteString -> C.ByteString -> IO Connection
connectToDB url port user pass = do
  let connSettings = settings url port user pass ""
  connResult <- acquire connSettings
  case connResult of
    Left (Just errMsg) -> error $ C.unpack errMsg
    Left Nothing -> error "Unspecified connection error!"
    Right connection -> return connection

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

initDB :: Connection -> IO ()
initDB conn = runSession conn $ statement () createSubbmissionsTableStatement

runSession :: Connection -> Session a -> IO a
runSession conn session = do
  sessionResult <- run session conn
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

addSubmission :: Connection -> T.Text -> T.Text -> Int16 -> IO ()
addSubmission conn name txt score = do
  zonedTimestamp <- getZonedTime
  let localTimestamp = zonedTimeToLocalTime zonedTimestamp
  runSession conn $
    statement (localTimestamp, name, txt, score) insertSubmissionStatement
