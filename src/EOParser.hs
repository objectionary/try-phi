module EOParser
  ( parseProgram,
    parseIndexedProgram,
    parseTermProgram,
    PrettyPrintTerm.pprintTermProgram,
    PrettyPrintTree.pprintTree,
    Data.Text.pack,
    Ann (..),
    AttachedName (..),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (..),
    SuffixName (..),
    THeadTerminal(..),
    AttachedOrArgument (AttachedOrArgument),
    Options3 (..),
    Term (..),
    Head(..),
    HeadName(..),
    LetterName(..),
    Modifier(..), DataValue (..)
  )
where

import Data.Text (Text,pack)
import EnumerateNodes (getIndexedProgram)
import ParseEO as A (TProgram, tProgram, THeadTerminal(..))
import PrettyPrintTerm (pprintTermProgram)
import PrettyPrintTree (pprintTree)
import Text.Megaparsec (parseMaybe)
import ToTerm (
    getTermProgram,
    Ann (..),
    AttachedName (..),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (..),
    SuffixName (..),
    Ann (..),
    AttachedName (..),
    AttachedOrArgument (AttachedOrArgument),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (Opt2B),
    Options3 (..),
    SuffixName (..),
    Term (App, Dot, HeadTerm, Obj),
    Head(..),
    HeadName(..),
    LetterName(..),
    Modifier(..), DataValue (..))

parseProgram :: Text -> Maybe TProgram
parseProgram = parseMaybe tProgram

parseIndexedProgram :: Text -> Maybe TProgram
parseIndexedProgram p = getIndexedProgram <$> parseProgram p

parseTermProgram :: Text -> Maybe Term
parseTermProgram p = getTermProgram <$> parseIndexedProgram p
