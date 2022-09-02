module Grading
  ( grade
  ) where

import Control.Monad
import qualified Data.Text as T
import Spelling

misspellingsToGrade :: Int -> Int
misspellingsToGrade n
  | n < 0 = 5
  | n > 5 = 0
  | otherwise = 5 - n

grade :: T.Text -> IO Int
grade = return . misspellingsToGrade <=< countMisspellings
