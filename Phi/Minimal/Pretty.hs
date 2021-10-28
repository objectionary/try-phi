{-# OPTIONS_GHC -Wall -fno-warn-orphans        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Phi.Minimal.Pretty where

import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.HashMap.Strict.InsOrd     as InsOrdHashMap
import           Data.Text.Prettyprint.Doc      as Doc

import           Phi.Minimal.Machine.CallByName
import           Phi.Minimal.Model

instance Show Term where show = show . pretty
instance Pretty Term where pretty = ppTerm

ppConfiguration :: Configuration -> Doc ann
ppConfiguration Configuration{..} =
  encloseSepAfter "< " " >" (" " <> Doc.semi <> " ")
    [ maybe "ɛ" ppTerm currentTerm, ppActions actions, ppEnvironment environment ]

ppActions :: [Action] -> Doc ann
ppActions [] = "ɛ"
ppActions as = foldMap ppAction as

ppAction :: Action -> Doc ann
ppAction (DotAction a) = "." <> pretty a
ppAction (AppAction (a, (u, e)))
  = group (pretty a <+> "↦") <+> tupled [ ppTerm u, ppEnvironment e ]

ppEnvironment :: Environment -> Doc ann
ppEnvironment parents
  = foldMap (\parent -> ppParent parent <> ":") parents <> "ɛ"

ppParent :: Parent -> Doc ann
ppParent Parent{..} = tupled
  [ ppObj original, ppApplications applications ]

ppApplications :: HashMap Attr (Term, Environment) -> Doc ann
ppApplications o
  | null o = "⟦⟧"
  | otherwise
    = group . nest 2 . encloseSepAfter ("⟦" <> line) (nest (-2) (line <> "⟧")) (comma <> line)
    . map (ppAction . AppAction) . HashMap.toList
    $ o

ppTerm :: Term -> Doc ann
ppTerm = \case
  Obj o -> ppObj o
  Dot t a -> ppTerm t <> dot <> pretty a
  App t (a, u) ->
    ppTerm t <> parens (ppAttrWithValue (a, Attached u))
  Loc n -> ppLoc n

encloseSepAfter
  :: Doc ann
  -> Doc ann
  -> Doc ann
  -> [Doc ann]
  -> Doc ann
encloseSepAfter bra ket separator = \case
  []    -> bra <> ket
  [doc] -> bra <> doc <> ket
  docs  -> bra <> mconcat (addSepAfter docs) <> ket
  where
    addSepAfter []         = []
    addSepAfter [doc]      = [doc]
    addSepAfter (doc:docs) = (doc <> separator) : addSepAfter docs

ppObj :: Object Term -> Doc ann
ppObj o
  | null o = "⟦⟧"
  | otherwise
    = group . nest 2 . encloseSepAfter ("⟦" <> line) (nest (-2) (line <> "⟧")) (comma <> line)
    . map ppAttrWithValue . InsOrdHashMap.toList . getObject
    $ o

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) = group $
  group (pretty a <+> "↦") <+> ppAttrValue value

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue = \case
  VoidAttr -> "ø"
  Attached t -> ppTerm t

ppLoc :: Int -> Doc ann
ppLoc n = pretty ("ρ" <> n')
  where
    n' = map toSuperscript (show n)
    toSuperscript c =
      case lookup c (zip "1234567890" "¹²³⁴⁵⁶⁷⁸⁹⁰") of
        Just c' -> c'
        _       -> c

