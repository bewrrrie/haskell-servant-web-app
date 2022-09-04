{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module App
  ( run
  ) where

import Control.Monad.Trans
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)
import Grading
import qualified Network.Wai.Handler.Warp as W
import Servant

data SubmitInfo =
  SubmitInfo
    { text :: T.Text
    , userId :: T.Text
    }
  deriving (Generic, Show, Eq)

instance ToJSON SubmitInfo

instance FromJSON SubmitInfo

type TextReviewAPI
   = "submit" :> ReqBody '[ JSON] SubmitInfo :> Post '[ JSON] TextReview

server :: Server TextReviewAPI
server body = liftIO $ grade txt
  where
    txt = text body
    uid = userId body

textReviewAPI :: Proxy TextReviewAPI
textReviewAPI = Proxy

app :: Application
app = serve textReviewAPI server

run :: Int -> IO ()
run port = W.run port app
