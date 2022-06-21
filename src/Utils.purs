module Utils(class_, attr_, classes_) where

import Prelude

import Data.Maybe (Maybe)
import Halogen (AttrName(..))
import Halogen.HTML (HTML) as HH
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))

class_ n = HP.class_ $ ClassName n
attr_ k v = HP.attr (AttrName k) v

classes_ n = HP.classes $ ClassName <$> n
