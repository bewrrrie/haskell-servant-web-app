{-# LANGUAGE DeriveGeneric #-}

module Grading
  ( grade
  , misspelled
  , score
  , TextReview
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Spelling

data TextReview =
  TextReview
    { misspelled :: [T.Text]
    , score :: Int
    }
  deriving (Generic, Eq, Show)

instance ToJSON TextReview

-- | @misspellingsToGrade @n is a function which converts number 
-- | of misspellings @n to a grade on scale from 0 to 5.
-- | Each misspelling decreases the grade by one.
--
-- Examples:
-- >>> misspellingsToGrade 0
-- 5
-- >>> misspellingsToGrade 1
-- 4
-- >>> misspellingsToGrade (-20)
-- 5
-- >>> misspellingsToGrade 100
-- 0
misspellingsToGrade :: Int -> Int
misspellingsToGrade n
  | n < 0 = 5
  | n > 5 = 0
  | otherwise = 5 - n

grade :: T.Text -> IO TextReview
grade txt = misspelledWords >>= k
  where
    misspelledWords = spell txt
    k = return . toReview
    toReview ws = TextReview ws $ misspellingsToGrade $ length ws
