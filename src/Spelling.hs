{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Spelling
  ( spell
  ) where

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
   = "checkText" :> QueryParam "lang" T.Text :> QueryParam "text" T.Text :> Get '[ JSON] [SpellResponse]

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
  deriving (Show, Generic, FromJSON)

checkText :: Maybe T.Text -> ClientM [SpellResponse]
checkText = client api $ Just $ T.pack lang
  where
    api :: Proxy SpellAPI
    api = Proxy
    lang = "ru,en"

spell :: T.Text -> IO [T.Text]
spell txt = do
  let yandexApiUrl = "https://speller.yandex.net/services/spellservice.json"
  manager' <- newManager tlsManagerSettings
  url <- parseBaseUrl yandexApiUrl
  res <- runClientM (checkText $ Just txt) (mkClientEnv manager' url)
  case res of
    Left err -> error $ "Speller: spell:" ++ show err
    Right responses -> return $ map word responses
