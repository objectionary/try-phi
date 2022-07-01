{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module TH (newOptions, MyResponse (..), MyRequest (..), GraphTab (..), TabId (..), TextTabs (..)) where

import Data.Aeson (Options (constructorTagModifier), defaultOptions)
import Data.Data (Data (toConstr), Typeable)

-- CBN with graph
data GraphTab = GraphTab
  { states :: [String],
    graphs :: [String]
  }
  deriving (Show, Data, Typeable)

data TextTabs = TextTabs {
    eo :: String,
    original_term :: String,
    whnf :: String,
    nf :: String,
    cbn_reduction :: String,
    cbn_with_tap :: String
} deriving (Show, Data, Typeable)

data MyResponse
  = OkResponse
      { code :: String,
        textTabs :: TextTabs,
        graphTab :: GraphTab
      }
  | ErrorResponse
      { error :: String
      }
  deriving (Show, Data, Typeable)

data MyRequest = MyRequest {code :: String} deriving (Eq, Show)

data TabId = TEO | TTerm | TWHNF | TNF | TCBNReduction | TCBNWithTAP | TCBNWithGraph

instance Show TabId where
  show TEO = "eo"
  show TTerm = "original_term"
  show TWHNF = "whnf"
  show TNF = "nf"
  show TCBNReduction = "cbn_reduction"
  show TCBNWithTAP = "cbn_with_tap"
  show TCBNWithGraph = "cbn_with_graph"

modifyName :: String -> String
modifyName s
  | s == show (toConstr (OkResponse {})) = "ok"
  | s == show (toConstr (ErrorResponse {})) = "error"
  | otherwise = "unknown"

{-
>>>modifyName "ErrorResponse"
"error"
-}

newOptions = defaultOptions {constructorTagModifier = modifyName}
