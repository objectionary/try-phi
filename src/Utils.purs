module Utils(class_, attr_, getHTML, someText, classes_) where

import Prelude

import Halogen (AttrName(..), ClassName(..))
import Halogen.HTML (HTML, Node) as HH
import Halogen.HTML.Properties as HP
import Web.HTML.Common (ClassName(..))


class_ n = HP.class_ $ ClassName n
attr_ k v = HP.attr (AttrName k) v

classes_ n = HP.classes $ ClassName <$> n


foreign import getHTML :: forall a b. HH.HTML a b -> String

foreign import someText :: Unit -> String
