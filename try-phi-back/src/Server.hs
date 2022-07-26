{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server
  ( startApp,
    app,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.TH
import Data.Data
import Data.Functor ((<&>))
import Data.Text
import Data.Text.IO as DT
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders), cors, simpleCors, simpleCorsResourcePolicy)
import Servant
import System.Environment (getEnv)
import System.IO.Error
import System.Random
import TH
import Text.StringRandom (stringRandomIO)
import Data.Either.Combinators(rightToMaybe)
import Common
    ( getTermFromEO,
      getTermFromPhi,
      ppGraphs,
      ppNF,
      ppPhi,
      ppPhiToEO,
      ppStates,
      ppTapSteps,
      ppWHNF,
      ppWHNFSteps, ppEOSource, ppPhiSource, ppPhiToLatex)
import EOParser (parseTermProgram)

arr = ["eo", "original_term", "whnf", "nf", "cbn_reduction", "cbn_with_tap"]

$(deriveJSON defaultOptions ''GraphTab)
$(deriveJSON defaultOptions ''TextTabs)
$(deriveJSON newOptions ''MyResponse)
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

stepLimit :: Int
stepLimit = 10

server :: Server API
server = han PhiEditor :<|> han EOEditor
  where
    han e r = Handler $ ExceptT $ handle e r
    handle :: Editor -> MyRequest -> IO (Either ServerError MyResponse)
    handle ed (MyRequest r) =
      do
        let phiTerm =
              case ed of
                EOEditor -> getTermFromEO r
                PhiEditor -> getTermFromPhi r

            editorPrefix = getStr' $
              case ed of
                EOEditor -> "eo"
                PhiEditor -> "phi"
        return $
          case phiTerm of
            Left l -> Right (ErrorResponse l)
            Right s -> do
              let tt = TextTabs {
                      original_term = ppPhi s,
                      whnf = ppWHNF s,
                      nf = ppNF s,
                      cbn_reduction = ppWHNFSteps s,
                      cbn_with_tap = ppTapSteps s,
                      phi_latex = ppPhiToLatex s
                    }
                  gt = GraphTab {
                      states = ppStates stepLimit s,
                      graphs = ppGraphs stepLimit s
                    }
                  g' =
                    case ed of
                      PhiEditor -> ppEOSource s
                      EOEditor -> ppPhiSource s
              return $ OkResponse g' tt gt

    getStr' a = stringRandomIO "20\\d\\d-(1[0-2]|0[1-9])-(0[1-9]|1\\d|2[0-8])" <&> unpack <&> (a <>)
