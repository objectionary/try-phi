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
import Text.Megaparsec (parse, ParseErrorBundle)
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
import Data.Void (Void)

pprintCST :: forall a. (ShowIndented a) => a -> String
pprintCST = PrettyPrintTree.pprintTree

-- FIXME 
-- parseCSTProgram :: Text -> Either (ParseErrorBundle Text) TProgram
parseCSTProgram :: Text -> Either (ParseErrorBundle Text Void) TProgram
parseCSTProgram p = getIndexedProgram <$> parse tProgram "" p

parseTermProgram :: Text -> Either (ParseErrorBundle Text Void) Term
parseTermProgram p = getTermProgram <$> parseCSTProgram p
