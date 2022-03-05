{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module ParseIndents (pItemList, eof) where

import Control.Applicative hiding (some)
import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pItemList :: Parser (String, [String])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome (Just (mkPos 5)) (return . (header, )) pItem)

pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

-- main :: IO ()
-- main = print "ok"