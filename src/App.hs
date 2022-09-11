{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module App
  ( run
  ) where

import Control.Monad.Trans

import Data.Aeson
import qualified Data.Text as T

import GHC.Generics (Generic)
import Grading
import Hasql.Connection (Connection)
import qualified Network.Wai.Handler.Warp as W
import Servant
import SubmissionsDB

data SubmitInfo =
  SubmitInfo
    { userName :: T.Text
    , text :: T.Text
    }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

type TextReviewAPI
   = "submit" :> ReqBody '[ JSON] SubmitInfo :> Post '[ JSON] TextReview

getServer :: Connection -> Server TextReviewAPI
getServer conn body =
  let txt = text body
      name = userName body
   in liftIO $ do
        gr <- grade txt
        addSubmission conn name txt (score gr)
        return gr

textReviewAPI :: Proxy TextReviewAPI
textReviewAPI = Proxy

app :: Connection -> Application
app conn = serve textReviewAPI server
  where
    server = getServer conn

run :: Connection -> Int -> IO ()
run conn port = W.run port (app conn)
