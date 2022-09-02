{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Speller
  ( spell
  ) where

import Data.Aeson
import Data.Data (Proxy(Proxy))
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
    , word :: [Char]
    , s :: [[Char]]
    }
  deriving (Show, Generic)

instance FromJSON SpellResponse

checkText :: Maybe T.Text -> ClientM [SpellResponse]
checkText = client api
  where
    api :: Proxy SpellAPI
    api = Proxy

spell :: T.Text -> IO [SpellResponse]
spell txt = do
  let yandexApiUrl = "https://speller.yandex.net/services/spellservice.json"
  manager' <- newManager tlsManagerSettings
  url <- parseBaseUrl yandexApiUrl
  res <- runClientM (checkText $ Just txt) (mkClientEnv manager' url)
  case res of
    Left err -> error $ show err
    Right response -> return response
