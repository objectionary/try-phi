module Phi.Minimal.Model.Types where

import           Control.Unification
import           Control.Unification.IntVar
import           Data.HashMap.Strict        (HashMap)

import           Phi.Minimal.Model

newtype ObjectType a = ObjectType
  { getObjectType :: HashMap Attr a }

type Type = UTerm ObjectType IntVar

