{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Network.Wai.Middleware.Cors (simpleCors, CorsResourcePolicy (corsOrigins, corsMethods, corsRequestHeaders), cors, simpleCorsResourcePolicy)
import qualified Faker.Yoda as Yoda
import Control.Monad.IO.Class
import Faker
import Data.Text.IO as DT
import Data.Text
import Data.Functor ((<&>))
import System.Environment (getEnv)
import System.IO.Error

arr = ["eo", "original_term", "whnf", "nf", "cbn_reduction", "cbn_with_tap", "cbn_with_graph"]

data Tabs = Tabs {
    eo :: String,
    original_term :: String,
    whnf :: String,
    nf :: String,
    cbn_reduction :: String,
    cbn_with_tap :: String,
    cbn_with_graph :: String
  } deriving (Eq, Show)

data MyResponse = MyResponse {
    code :: String,
    tabs :: Tabs
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Tabs)
$(deriveJSON defaultOptions ''MyResponse)

type API = "phi" :> Put '[JSON] MyResponse

startApp :: IO ()
startApp = catchIOError 
  (do
    port <- read <$> getEnv "Port"
    print port
    run port app)
  (\_ -> run 8082 app)



corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
      -- CORS origin complaint
      -- https://stackoverflow.com/q/63876281
      policy = simpleCorsResourcePolicy
        {
            corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ],
            corsOrigins = Just (["http://localhost:1234"], True),
            corsRequestHeaders = [ "authorization", "content-type" ]
        }

app :: Application
app = corsPolicy $ serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =
    do
      tabs1 <- liftIO $ Tabs <$> getStr <*> getStr <*> getStr <*> getStr <*> getStr <*> getStr <*> getStr
      liftIO $ MyResponse <$> getStr <*> return tabs1

getStr :: IO String
getStr = generateWithSettings (setNonDeterministic defaultFakerSettings) Yoda.quotes <&> unpack

resp = MyResponse {
    code = "code",
    tabs = Tabs {
      eo = "eo",
      original_term = "original_term",
      whnf = "whnf",
      nf = "nf",
      cbn_reduction = "cbn_reduction",
      cbn_with_tap = "cbn_with_tap",
      cbn_with_graph = "cbn_with_graph"
    }
  }
