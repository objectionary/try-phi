module Utils(class_, attr_, getHTML, someText) where

import Prelude

import Halogen (AttrName(..))
import Halogen.HTML (HTML, Node) as HH
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))


class_ n = HP.class_ $ ClassName n
attr_ k v = HP.attr (AttrName k) v


foreign import getHTML :: forall a b. HH.HTML a b -> String

foreign import someText :: Unit -> String
