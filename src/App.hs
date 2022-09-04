{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App
  ( run
  ) where

import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import Grading
import qualified Network.Wai.Handler.Warp as W
import Servant

type TextReviewAPI
   = "check" :> QueryParam "text" T.Text :> Post '[ JSON] TextReview

server :: Server TextReviewAPI
server = liftIO . grade . fromJust

textReviewAPI :: Proxy TextReviewAPI
textReviewAPI = Proxy

app :: Application
app = serve textReviewAPI server

run :: Int -> IO ()
run port = W.run port app
