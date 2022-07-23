{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Phi.Minimal.PPToLatex() where

import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Phi.Minimal.Model
    ( Term(..),
      DataValue(..),
      AttrValue(..),
      Object(getObject),
      Attr )
import Prettyprinter as Doc hiding (indent)
import Data.List (intercalate)
import Control.Monad.State (State, MonadState (state, get))
import Data.Function ((&))

{-
& \ff{book2}(\ff{isbn}) \mapsto \llbracket \br
& \quad \ff{title} \mapsto \ff{"Object Thinking"}, \br
& \quad \ff{price} \mapsto \ff{memory} \br
& \rrbracket. \\

https://arxiv.org/pdf/2111.13384.pdf#page=8
-}

data St ann = St { indent :: Int, result :: Res ann}

newtype Res ann = Res {content :: Doc ann}

type MyState ann = State (St ann) (Res ann)

-- instance Show Term where
--   show = show . pretty

-- instance Pretty Term where
--   pretty = ppTerm

-- instance Show (AttrValue Term) where
--   show = show . pretty

-- instance Pretty (AttrValue Term) where
--   pretty = ppAttrValue

-- ppTerm :: Term -> MyState ann
-- ppTerm =
--   \case
--     Obj o -> ppObj o
--     Dot t a -> ppTerm t <> dot <> ppAttr a
--     App t (a, u) -> ppTerm t <> parens (ppAttrWithValue (a, Attached u))
--     Loc n -> ppLoc n
--     DataTerm t ->
--       case t of
--         DataInteger i -> pretty i
--         NoData -> pretty $ show NoData

-- ppInt :: Integer -> MyState ann
-- ppInt = pretty

-- encloseSepAfter :: Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> MyState ann
-- encloseSepAfter bra ket separator =
--   \case
--     [] -> bra <> ket
--     [doc] -> bra <> doc <> ket
--     docs -> bra <> mconcat (addSepAfter docs) <> ket
--   where
--     addSepAfter [] = []
--     addSepAfter [doc] = [doc]
--     addSepAfter (doc : docs) = (doc <> separator) : addSepAfter docs

-- llbracket :: Doc ann
-- llbracket = "\\llbracket"

-- rrbracket :: Doc ann
-- rrbracket = "\\rrbracket"

-- encBrackets :: Doc ann -> MyState ann
-- encBrackets t = do 
  
--   enclose llbracket rrbracket t

pre :: Doc ann -> Doc ann -> Doc ann
pre a b = a <+> b

amp :: Doc ann
amp = "&"

quad :: Doc ann
quad = "\\quad"


-- ppObj :: Object Term -> MyState ann
-- ppObj o
--   | null (getObject o) = encBrackets ""
--   | otherwise = return $
--     group
--       . nest 2
--       . encloseSepAfter (llbracket <> line <> amp) (nest (-2) (line <> rrbracket)) (comma <> line)
--       . map ppAttrWithValue
--       . InsOrdHashMap.toList
--       . getObject
--       $ o

-- ff :: Doc ann -> MyState ann
-- ff t = do
--   enclose "\\ff{" "}" t

-- liftState :: (a -> a) -> MyState ann
-- liftState f = do
--   St {..} <- get
--   return _

-- ppAttrWithValue :: (Attr, AttrValue Term) -> MyState ann
-- ppAttrWithValue (a', value) = return $ group $ group (ff (ppAttr a') <+> "\\mapsto") <+> ppAttrValue value

-- ppAttr :: Attr -> MyState ann
-- ppAttr a' = return f
--   where 
--     f
--       | a' == "@" = "𝜑"
--       | otherwise = pretty a

ppAttrValue :: AttrValue Term -> MyState ann
ppAttrValue =
  \case
    VoidAttr -> "ø"
    Attached t' -> ppTerm t'

ppLoc :: Int -> MyState ann
ppLoc n = return $ Res {content = pretty (intercalate "." (replicate n "\\rho"))}