{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat
import GHC.Generics

import Control.Monad.Except
import Control.Monad.Reader

import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Time
import Data.String.Conversions

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time

import Elm (Spec (Spec), specsToDir, toElmDecoderSource, toElmTypeSource)
import Servant.Elm (ElmType, Proxy (Proxy), defElmImports, generateElmForAPI)

import Time
import Workout

import Data.Time.Clock
import Data.Time.Calendar.WeekDate

type API = "workout" :> Get '[JSON] Workout
  :<|> "reps" :> ReqBody '[JSON] Int :> Post '[JSON] Workout

myConnectInfo = defaultConnectInfo
  { connectUser = "dev"
  , connectDatabase = "allpro"
  }

server :: Server API
server = workout
  :<|> reps

workout = do
  let user = 1
  conn <- liftIO $ connect myConnectInfo
  queryWorkout conn user

reps :: Int -> Handler Workout
reps r = do
  let user = 1
  conn <- liftIO $ connect myConnectInfo
  setReps conn user r

selectWorkout :: Query
selectWorkout = "select cycle, cyclestart from workout where id = ?"

selectWeights :: Query
selectWeights = "select squat, bench, bentrow, overhead, deadlift, curl, calves from weights"

updateStart :: Query
updateStart = "update workout set start = ? where id = ?"

queryWorkout :: Connection -> Int -> Handler Workout
queryWorkout conn user = do
  [(cycle, start)] <- liftIO $ query conn selectWorkout ( Only user )
  [(squat, bench, bentrow, overhead, deadlift, curl, calves)] <- liftIO $ query_ conn selectWeights
  let weights = [squat, bench, bentrow, overhead, deadlift, curl, calves]
  return $ Workout cycle start weights

setReps :: Connection -> Int -> Int -> Handler Workout
setReps conn user r = do
  [ (_, start ) ] :: [ ( Int, UTCTime ) ]  <- liftIO $ query conn selectWorkout $ Only user
  now <- liftIO getCurrentTime
  liftIO $ execute conn updateStart ( newstart now r :: UTCTime, user :: Int )
  queryWorkout conn user

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = simpleCors $ serve userAPI server

instance ElmType Workout

spec :: Spec
spec = Spec ["AllproApi"]
  (defElmImports
   : toElmTypeSource (Proxy :: Proxy Workout)
   : toElmDecoderSource (Proxy :: Proxy Workout)
   : generateElmForAPI (Proxy :: Proxy API))

main :: IO ()
main = do
  specsToDir [spec] "elm"
  run 8082 app
