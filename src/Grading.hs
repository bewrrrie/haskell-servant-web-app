{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Grading
  ( grade
  , misspelled
  , score
  , TextReview
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import GHC.Int
import Spelling

data TextReview =
  TextReview
    { misspelled :: [T.Text]
    , score :: Int16
    }
  deriving (Generic, Eq, Show, ToJSON)

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
misspellingsToGrade :: Int -> Int16
misspellingsToGrade n
  | n < 0 = 5
  | n > 5 = 0
  | otherwise = 5 - fromIntegral n

-- | @grade @txt is a function which converts text @txt
-- | to a @TextReview object that contains the list of
-- | misspelled words and the score computed from the
-- | number of misspellings.
--
-- import qualified Data.Text as T
-- Examples:
-- >>> grade $ T.pack ""
-- TextReview {misspelled = [], score = 5}
-- >>> grade $ T.pack "A catt and a dogg."
-- TextReview {misspelled = ["catt","dogg"], score = 3}
-- >>> grade $ T.pack "A cat and a dog."
-- TextReview {misspelled = [], score = 5}
-- >>> grade $ T.pack "Dont do this!"
-- TextReview {misspelled = ["Dont"], score = 4}
-- >>> grade $ T.pack "Don't do this!"
-- TextReview {misspelled = [], score = 5}
grade :: T.Text -> IO TextReview
grade txt = toReview <$> misspelledWords
  where
    misspelledWords = spell txt
    toReview ws = TextReview ws $ misspellingsToGrade $ length ws
