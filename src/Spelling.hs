{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Spelling
  ( spell
  , countMisspellings
  ) where

import Control.Monad
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

-- Yandex Speller API (see https://yandex.ru/dev/speller/)
type SpellAPI
   = "checkText" :> QueryParam "text" T.Text :> Get '[ JSON] [SpellResponse]

-- Response description
-- (see https://yandex.ru/dev/speller/doc/dg/reference/checkText.html)
data SpellResponse =
  SpellResponse
    { code :: Int
    , pos :: Int
    , row :: Int
    , col :: Int
    , len :: Int
    , word :: T.Text
    , s :: [T.Text]
    }
  deriving (Show, Generic)

instance FromJSON SpellResponse

-- Misspelling data type:
-- each value '(w,ws)' represents a misspelled word 'w'
-- together with a list 'ws' of all its possible fixes
type Misspelling = (T.Text, [T.Text])

responseToMisspelling :: SpellResponse -> Misspelling
responseToMisspelling r = (word r, s r)

checkText :: Maybe T.Text -> ClientM [SpellResponse]
checkText = client api
  where
    api :: Proxy SpellAPI
    api = Proxy

spell :: T.Text -> IO [Misspelling]
spell txt = do
  let yandexApiUrl = "https://speller.yandex.net/services/spellservice.json"
  manager' <- newManager tlsManagerSettings
  url <- parseBaseUrl yandexApiUrl
  res <- runClientM (checkText $ Just txt) (mkClientEnv manager' url)
  case res of
    Left err -> error $ "Speller: spell:" ++ show err
    Right responses -> return $ map responseToMisspelling responses

countMisspellings :: T.Text -> IO Int
countMisspellings = return . length <=< spell
