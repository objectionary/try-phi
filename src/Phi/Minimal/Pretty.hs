{-# OPTIONS_GHC -Wall -fno-warn-orphans#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Phi.Minimal.Pretty where

import           Data.Graph.Inductive.PatriciaTree    (Gr)
import           Data.HashMap.Strict.InsOrd           (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd           as InsOrdHashMap

import           Prettyprinter            as Doc
import           Phi.Minimal.Machine.CallByName
import qualified Phi.Minimal.Machine.CallByName.Graph as Graph
import           Phi.Minimal.Model

instance Show Term where
  show = show . pretty

instance Pretty Term where
  pretty = ppTerm

instance Show (AttrValue Term) where
  show = show . pretty

instance Pretty (AttrValue Term) where
  pretty = ppAttrValue

ppTerm :: Term -> Doc ann
ppTerm =
  \case
    Obj o -> ppObj o
    Dot t a -> ppTerm t <> dot <> pretty a
    App t (a, u) -> ppTerm t <> parens (ppAttrWithValue (a, Attached u))
    Loc n -> ppLoc n

encloseSepAfter :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepAfter bra ket separator =
  \case
    [] -> bra <> ket
    [doc] -> bra <> doc <> ket
    docs -> bra <> mconcat (addSepAfter docs) <> ket
  where
    addSepAfter []         = []
    addSepAfter [doc]      = [doc]
    addSepAfter (doc:docs) = (doc <> separator) : addSepAfter docs

ppObj :: Object Term -> Doc ann
ppObj o
  | null (getObject o) = "⟦⟧"
  | otherwise =
    group .
    nest 2 .
    encloseSepAfter ("⟦" <> line) (nest (-2) (line <> "⟧")) (comma <> line) .
    map ppAttrWithValue . InsOrdHashMap.toList . getObject $
    o

ppAttrWithValue :: (Attr, AttrValue Term) -> Doc ann
ppAttrWithValue (a, value) =
  group $ group (pretty a <+> "↦") <+> ppAttrValue value

ppAttrValue :: AttrValue Term -> Doc ann
ppAttrValue =
  \case
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

ppWhnfSteps :: Term -> Doc ann
ppWhnfSteps term =
  encloseSepAfter "" "" hardline $ zipWith ppStep [1 :: Int ..] (whnfSteps term)
  where
    ppStep i t = Doc.fill 5 (Doc.brackets (pretty i)) <+> align (ppTerm t)

-- * Call-by-name term reduction machine
ppStepsFor :: Term -> Doc ann
ppStepsFor term =
  encloseSepAfter "" "" hardline $
  zipWith ppStep [1 :: Int ..] (steps (initConfiguration term))
  where
    ppStep i conf =
      Doc.fill 5 (Doc.brackets (pretty i)) <+> align (ppConfiguration conf)

ppConfiguration :: Configuration -> Doc ann
ppConfiguration Configuration {..} =
  encloseSepAfter
    "< "
    " >"
    (" " <> Doc.semi <> " ")
    [maybe "ɛ" ppTerm currentTerm, ppActions actions, ppEnvironment environment]

ppActions :: [Action] -> Doc ann
ppActions [] = "ɛ"
ppActions as = foldMap ppAction as

ppAction :: Action -> Doc ann
ppAction (DotAction a) = "." <> pretty a
ppAction (AppAction (a, (u, e))) =
  parens (group (pretty a <+> "↦") <+> tupled [ppTerm u, ppEnvironment e])

ppEnvironment :: Environment -> Doc ann
ppEnvironment parents =
  foldMap (\parent -> ppParent parent <> ":") parents <> "ɛ"

ppParent :: Parent -> Doc ann
ppParent Parent {..} = tupled [ppObj original, ppApplications applications]

ppApplications :: InsOrdHashMap Attr (Term, Environment) -> Doc ann
ppApplications o
  | null o = "⟦⟧"
  | otherwise =
    group .
    nest 2 .
    encloseSepAfter ("⟦" <> line) (nest (-2) (line <> "⟧")) (comma <> line) .
    map (ppAction . AppAction) . InsOrdHashMap.toList $
    o

-- * Call-by-name graph-assisted evaluation machine
ppGraphStepsFor :: Term -> Int -> Doc ann
ppGraphStepsFor term stepNumber =
  encloseSepAfter "" "" hardline $
  zipWith ppStep [1 :: Int ..] (Graph.steps (Graph.initConfiguration @Gr term))
  where
    arrowPointer i =
      Doc.pretty $
      if i - 1 == stepNumber
        then "->"
        else "" :: String
    ppStep i conf =
      Doc.fill 5 (Doc.brackets (pretty i)) <+>
      Doc.fill 5 (align (arrowPointer i)) <+> align (ppGraphConfiguration conf)

ppGraphConfiguration' :: Graph.Configuration Gr -> Doc ann
ppGraphConfiguration' Graph.Configuration {..} =
  encloseSepAfter
    "< "
    " >"
    (" " <> Doc.semi <> " ")
    [ maybe "ɛ" pretty currentNode
    , ppGraphActions actions
    , ppGraphEnvironment environment
    ]

ppGraphConfiguration :: Graph.Configuration gr -> Doc ann
ppGraphConfiguration Graph.Configuration {..} =
  encloseSepAfter
    "< "
    " >"
    (" " <> Doc.semi <> " ")
    [ maybe "ɛ" pretty currentNode
    , ppGraphActions actions
    , ppGraphEnvironment environment
    ]

ppGraphActions :: [Graph.Action] -> Doc ann
ppGraphActions [] = "ɛ"
ppGraphActions as = foldMap ppGraphAction as

ppGraphAction :: Graph.Action -> Doc ann
ppGraphAction =
  \case
    Graph.DotAction (_, a) -> "." <> pretty a
    Graph.AppAction (n, e) -> tupled [pretty n, ppGraphEnvironment e]
    Graph.LocAction (_, n) -> ppLoc n

ppGraphEnvironment :: Graph.Environment -> Doc ann
ppGraphEnvironment parents =
  foldMap (\parent -> ppGraphParent parent <> ":") parents <> "ɛ"

ppGraphParent :: Graph.Parent -> Doc ann
ppGraphParent Graph.Parent {..} =
  encloseSep "" "" "▸" $ map ppGraphCopy copies ++ [pretty original]
  where
    ppGraphCopy (n, e) = tupled [pretty n, ppGraphEnvironment e]
