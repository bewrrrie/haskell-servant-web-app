{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module App
  ( run
  ) where

import Control.Monad.Trans (liftIO)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T

import GHC.Generics (Generic)
import Grading (TextReview, grade, score)
import Hasql.Pool (Pool)
import qualified Network.Wai.Handler.Warp as W
import Servant
  ( (:>)
  , Application
  , JSON
  , Post
  , Proxy(Proxy)
  , ReqBody
  , Server
  , serve
  )
import SubmissionsDB (addSubmission)

data SubmitInfo =
  SubmitInfo
    { userName :: !T.Text
    , text :: !T.Text
    }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

type TextReviewAPI
   = "submit" :> ReqBody '[ JSON] SubmitInfo :> Post '[ JSON] TextReview

getServer :: Pool -> Server TextReviewAPI
getServer pool body =
  let txt = text body
      name = userName body
   in liftIO $ do
        gr <- grade txt
        addSubmission pool name txt (score gr)
        return gr

textReviewAPI :: Proxy TextReviewAPI
textReviewAPI = Proxy

app :: Pool -> Application
app pool = serve textReviewAPI server
  where
    server = getServer pool

run :: Pool -> Int -> IO ()
run pool port = W.run port (app pool)
