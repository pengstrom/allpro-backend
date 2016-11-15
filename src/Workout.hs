{-# LANGUAGE DeriveGeneric #-}

module Workout ( Workout(..) ) where

import Data.Time.Clock
import GHC.Generics
import Database.PostgreSQL.Simple.Time
import Data.Aeson.Types

data Workout = Workout
  { cycle :: Int
  , start :: UTCTime
  , weights :: [ Double ]
  }
  deriving (Show, Generic)

instance ToJSON Workout

