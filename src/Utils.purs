module Utils(class_, attr_, classes_, writeText, Clipboard, clipboard, makePermalink, setGlobalBoolean, readGlobalBoolean) where

import Prelude

import Data.Maybe (Maybe(..))
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

foreign import setGlobalBoolean :: String -> Boolean -> Effect Unit


-- https://book.purescript.org/chapter10.html#beyond-simple-types
readGlobalBoolean :: String -> Effect (Maybe Boolean)
readGlobalBoolean s = readGlobalBooleanImpl s Just Nothing
foreign import readGlobalBooleanImpl :: String -> (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Effect (Maybe Boolean)