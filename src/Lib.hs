{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lib
  ( startApp,
    app,
  )
where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.Functor ((<&>))
import Data.Text
import Data.Text.IO as DT
import Faker
import qualified Faker.Yoda as Yoda
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders), cors, simpleCors, simpleCorsResourcePolicy)
import Servant
import System.Environment (getEnv)
import System.IO.Error
import Text.StringRandom (stringRandomIO)

arr = ["eo", "original_term", "whnf", "nf", "cbn_reduction", "cbn_with_tap", "cbn_with_graph"]

data Tabs = Tabs
  { eo :: String,
    original_term :: String,
    whnf :: String,
    nf :: String,
    cbn_reduction :: String,
    cbn_with_tap :: String,
    cbn_with_graph :: String
  }
  deriving (Eq, Show)

data MyResponse = MyResponse
  { code :: String,
    tabs :: Tabs
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Tabs)
$(deriveJSON defaultOptions ''MyResponse)

data MyRequest = MyRequest{code :: String} deriving (Eq, Show)

$(deriveJSON defaultOptions ''MyRequest)

type API =
  "phi" :> ReqBody '[JSON] MyRequest :> Put '[JSON] MyResponse
    :<|> "eo" :> ReqBody '[JSON] MyRequest :> Put '[JSON] MyResponse

startApp :: IO ()
startApp =
  catchIOError
    ( do
        port <- read <$> getEnv "PORT"
        print port
        run port app
    )
    (\_ -> run 8082 app)

corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
  where
    -- CORS origin complaint
    -- https://stackoverflow.com/q/63876281
    policy =
      simpleCorsResourcePolicy
        { corsMethods = ["GET", "POST", "PUT", "OPTIONS"],
          corsOrigins =
            Just
              ( [ "https://br4ch1st0chr0n3.github.io/try-phi-front",
                  "https://br4ch1st0chr0n3.github.io/",
                  "https://br4ch1st0chr0n3.github.io",
                  "http://localhost:1234"
                ],
                True
              ),
          corsRequestHeaders = ["authorization", "content-type"]
        }

app :: Application
app = corsPolicy $ serve api server

api :: Proxy API
api = Proxy

data Editor = PhiEditor | EOEditor

server :: Server API
server = handle PhiEditor :<|> handle EOEditor
  where
    handle ed req =
      do
        let g = getStr' $
              case ed of
                PhiEditor -> "eo"
                EOEditor -> "phi"
        tabs1 <- liftIO $ Tabs <$> g <*> g <*> g <*> g <*> g <*> g <*> g
        liftIO $ MyResponse <$> g <*> return tabs1

    getStr' a = stringRandomIO "20\\d\\d-(1[0-2]|0[1-9])-(0[1-9]|1\\d|2[0-8])" <&> unpack <&> (a <>)

-- tabs1 <- liftIO $ Tabs <$> getStr <*> getStr <*> getStr <*> getStr <*> getStr <*> getStr <*> getStr
-- liftIO $ MyResponse <$> getStr <*> return tabs1


getStr :: IO String
getStr = generateWithSettings (setNonDeterministic defaultFakerSettings) Yoda.quotes <&> unpack

resp =
  MyResponse
    { code = "code",
      tabs =
        Tabs
          { eo = "eo",
            original_term = "original_term",
            whnf = "whnf",
            nf = "nf",
            cbn_reduction = "cbn_reduction",
            cbn_with_tap = "cbn_with_tap",
            cbn_with_graph = "cbn_with_graph"
          }
    }
