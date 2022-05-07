module EOParser
  ( parseProgram,
    parseIndexedProgram,
    parseTermProgram,
    PrettyPrintTerm.pprintTermProgram,
    PrettyPrintTree.pprintTree,
    Data.Text.pack
  )
where

import Data.Text (Text,pack)
import EnumerateNodes (getIndexedProgram)
import ParseEO as A (I, TProgram, tProgram)
import PrettyPrintTerm (pprintTermProgram)
import PrettyPrintTree (pprintTree)
import Text.Megaparsec (parseMaybe)
import ToTerm (K, Term, getTermProgram)

parseProgram :: Text -> Maybe (I TProgram)
parseProgram = parseMaybe tProgram

parseIndexedProgram :: Text -> Maybe (I TProgram)
parseIndexedProgram p = getIndexedProgram <$> parseProgram p

parseTermProgram :: Text -> Maybe (K Term)
parseTermProgram p = getTermProgram <$> parseIndexedProgram p
