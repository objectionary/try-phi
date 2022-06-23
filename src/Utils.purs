module Utils(class_, attr_, classes_, writeText, Clipboard, clipboard, makePermalink) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Halogen (AttrName(..))
import Halogen.HTML (HTML) as HH
import Halogen.HTML.Properties as HP
import Web.DOM (Element)
import Web.HTML (Navigator)
import Web.HTML.Common (ClassName(..))

class_ n = HP.class_ $ ClassName n
attr_ k v = HP.attr (AttrName k) v

classes_ n = HP.classes $ ClassName <$> n

foreign import data Clipboard :: Type

foreign import clipboard :: Navigator -> Effect Clipboard

foreign import writeText :: Clipboard -> String -> Effect Unit

foreign import makePermalink :: String -> String -> Effect String