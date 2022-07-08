{-# LANGUAGE RankNTypes #-}
module EOParser
  ( module EOParser,
    module A,
    module PrettyPrintTerm,
    module PrettyPrintTree,
    module TT,
    pack
    )
where

import Data.Text (Text, pack)
import EnumerateNodes (getIndexedProgram)
import ParseEO as A (THeadTerminal (..), TProgram, tProgram)
import PrettyPrintTerm (pprintTermProgram)
import PrettyPrintTree (pprintTree, ShowIndented)
import Text.Megaparsec (parseMaybe)
import ToTerm (getTermProgram)
import ToTerm as TT
  ( Ann (..),
    AttachedName (..),
    AttachedOrArgument (..),
    DataValue (..),
    HasName (..),
    Head (..),
    HeadName (..),
    HeadTerminal (..),
    Label (..),
    LetterName (..),
    MethodName (..),
    Modifier (..),
    Options2 (..),
    Options3 (..),
    SuffixName (..),
    Term (..),
  )

pprintCST :: forall a. (ShowIndented a) => a -> String
pprintCST = PrettyPrintTree.pprintTree

-- FIXME 
parseCSTProgram :: Text -> Maybe TProgram
parseCSTProgram p = getIndexedProgram <$> parseMaybe tProgram p

parseTermProgram :: Text -> Maybe Term
parseTermProgram p = getTermProgram <$> parseCSTProgram p
