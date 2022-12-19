{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TH (newOptions, MyResponse (..), MyRequest (..), GraphTab (..), TabId (..), TextTabs (..), GetResponse (..)) where

import Data.Aeson (Options (constructorTagModifier), defaultOptions)
import Data.Aeson.Types
import Data.Data (Data (toConstr), Typeable)

-- CBN with graph
data GraphTab = GraphTab
  { states :: [String]
  , graphs :: [String]
  }
  deriving (Show, Data, Typeable)

data TextTabs = TextTabs
  { original_term :: String
  , whnf :: String
  , nf :: String
  , cbn_reduction :: String
  , cbn_with_tap :: String
  , phi_latex :: String
  }
  deriving (Show, Data, Typeable)

newtype GetResponse = GetResponse {resp :: String} deriving (ToJSON)

data MyResponse
  = OkResponse
      { code :: String
      , textTabs :: TextTabs
      , graphTab :: GraphTab
      }
  | ErrorResponse
      { error :: String
      }
  deriving (Show, Data, Typeable)

newtype MyRequest = MyRequest {code :: String} deriving (Eq, Show)

data TabId = TTerm | TWHNF | TNF | TCBNReduction | TCBNWithTAP | TCBNWithGraph | TPhiLatex

instance Show TabId where
  show TTerm = "original_term"
  show TWHNF = "whnf"
  show TNF = "nf"
  show TCBNReduction = "cbn_reduction"
  show TCBNWithTAP = "cbn_with_tap"
  show TCBNWithGraph = "cbn_with_graph"
  show TPhiLatex = "phi_latex"

modifyName :: String -> String
modifyName s
  | s == show (toConstr (OkResponse{})) = "ok"
  | s == show (toConstr (ErrorResponse{})) = "error"
  | otherwise = "unknown"

{-
>>>modifyName "ErrorResponse"
"error"
-}

newOptions = defaultOptions{constructorTagModifier = modifyName}
