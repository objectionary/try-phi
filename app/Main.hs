{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import           Control.Applicative  (Alternative ((<|>)))
import           Control.Monad        (guard)
import qualified Data.List            as DL
import           Data.Text            (Text, pack, unpack, singleton, head)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (notFollowedBy, takeWhile1P), Parsec,
                                       Stream (Tokens, Token), choice, many, manyTill,
                                       noneOf, parseTest, satisfy, some,
                                       takeWhileP, try, (<|>))
import           Text.Megaparsec.Char (alphaNumChar, char, crlf, eol, newline,
                                       printChar, string)

type Parser = Parsec Void Text

pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"



data TokenType =
  -- Non-terminals
    Abstraction
  | Application
  | Attribute
  | Attributes
  | Comment
  | Data
  | Has
  | Head
  | Htail
  | Label
  | License
  | Metas
  | Method
  | Object
  | Objects
  | Program
  | Suffix
  | Tail
  -- Terminals (regex-recognizable)
  | ARROW
  | AT
  | BOOL Text
  | BYTE Text
  | BYTES Text
  | CHAR Text
  | COLON
  | COMMENT Text
  | CONST
  | COPY
  | DOT
  | DOTS
  | EMPTY_BYTES
  | EOF
  | EOL Text
  | ESCAPE_SEQUENCE Text
  | EXPONENT Text
  | FLOAT Text
  | HASH
  | HEX
  | INDENT Int
  | INT Text
  | LB
  | LINEBREAK Text
  | LINE_BYTES
  | LSQ
  | META Text (Maybe Text)
  | MINUS
  | NAME Text
  | NEWLINE
  | PLUS
  | QUESTION
  | RB
  | REGEX Text Text
  | RHO
  | ROOT
  | RSQ
  | SIGMA
  | SLASH
  | SPACE
  | STAR
  | Text Text
  | TAB
  | TEXT Text
  | TEXT_MARK
  | UNTAB
  | VERTEX
  | XI
  | NONE
  deriving (Show)


data Node =
  Node {
    nodeToken :: TokenType
  , nodes     :: [Node]
  , range     :: Range
  }


tab :: String
tab = "|  "

type TabNumber = Int

printTree :: TabNumber -> Node -> String
printTree n node =
  DL.intercalate "" (replicate n tab)
  <> show (nodeToken node)
  <> show (range node)
  <> "\n"
  <> foldl (\s a -> s <> printTree (n + 1) a) "" (nodes node)

instance Show Node where
  show n = printTree 0 n


data Range =
  Range {
    startRow    :: Int
  , startColumn :: Int
  , endRow      :: Int
  , endColumn   :: Int
  }

instance Show Range where
  show (Range p1 p2 p3 p4) = "[" <> show (p1 + 1) <> ":" <> show (p2 + 1) <> ".." <> show (p3 + 1) <> ":" <> show (p4 + 1) <> "]"

data Position =
  Position {
    row    :: Int
  , column :: Int
  } deriving (Show)

initNode :: Node
initNode =
  Node {
    nodeToken = NONE
  , nodes = []
  , range = Range 0 0 0 0
  }

-- will later be split

pTerminal :: Text -> TokenType -> Position -> Parser Node
pTerminal s t p = do
  c <- string s
  let l = T.length c - 1
  return initNode {
    nodeToken = t
  , range = (rangeFrom p) {endColumn = column p + l - 1}
  }

cARROW :: Text
cARROW = "<"
cAT :: Text
cAT = "@"
cCOLON :: Text
cCOLON = ":"
cCONST :: Text
cCONST = "!"
cCOPY :: Text
cCOPY = "\'"
cDOT :: Text
cDOT = "."
cHASH :: Text
cHASH = "#"
cINDENT :: Text
cINDENT = "  "
cLB :: Text
cLB = "("
cLSQ :: Text
cLSQ = "["
cMINUS :: Text
cMINUS = "-"
cPLUS :: Text
cPLUS = "+"
cQUESTION :: Text
cQUESTION = "?"
cRB :: Text
cRB = ")"
cRHO :: Text
cRHO = "^"
cROOT :: Text
cROOT = "Q"
cRSQ :: Text
cRSQ = "]"
cSIGMA :: Text
cSIGMA = "&"
cSLASH :: Text
cSLASH = "/"
cSPACE :: Text
cSPACE = " "
cSTAR :: Text
cSTAR = "*"
cVERTEX :: Text
cVERTEX = "<"
cXI :: Text
cXI = "$"
cTEXT_MARK :: Text
cTEXT_MARK = "\"\"\""
cDOTS :: Text
cDOTS = "..."
cNEWLINE :: Text
cNEWLINE = "\n"
cCARET_RETURN :: Text
cCARET_RETURN = "\r"

inByteRange :: Char -> Bool
inByteRange c
  | '0' <= c && c <= '9' = True
  | 'A' <= c && c <= 'F' = True
  | otherwise = False

pBYTE :: Position -> Parser Node
pBYTE p = do
  s1 <- alphaNumChar
  guard (inByteRange s1)
  s2 <- alphaNumChar
  guard (inByteRange s2)
  let Position row column = p
  return initNode {
    nodeToken = BYTE (pack [s1, s2])
  , range = Range row column row (column + 1)
  }

-- takeP eats many tokens

pEMPTY_BYTES :: Position -> Parser Node
pEMPTY_BYTES = pTerminal "--" EMPTY_BYTES

pLINE_BYTES :: Position -> Parser Node
pLINE_BYTES p = do
    byte <- pBYTE p
    let bytes = some (string cMINUS *> pBYTE p)
    byteNodes <- bytes
    let indexed = DL.scanl correctPosition byte byteNodes
    let len = 2 + (length indexed - 1) * 3

    return initNode {
      nodeToken = LINE_BYTES
    , range = (range byte) {endColumn = column p + len - 1}
    , nodes = indexed
    }
    where
      correctPosition n a = correct a
        where
          aStartColumn = endColumn (range n) + 2
          aEndColumn = aStartColumn + 1
          correct x = x {range = (range x) {startColumn = aStartColumn, endColumn = aEndColumn}}

rangeFrom :: Position -> Range
rangeFrom p = Range (row p) (column p) (row p) (column p)

pCOMMENT :: Position -> Parser Node
pCOMMENT p = do
  _ <- string cHASH
  content <- pack <$>  (many printChar :: Parser String)
  let l = 1 + T.length content
  return initNode {
    nodeToken = COMMENT content
  , range = (rangeFrom p) {endColumn = column p + l - 1}
  }

pMETA :: Position -> Parser Node
pMETA p = do
  _ <- string cPLUS
  name <- pack <$> (many alphaNumChar :: Parser String)
  suffix <- pack <$> try (char ' ' *> many printChar :: Parser String)
  let mSuffix =
        case suffix of
          empty -> Nothing
          _  -> Just suffix
  let l = 1 + T.length name + maybe 0 (\s -> 1 + T.length s) mSuffix
  return initNode {
    nodeToken = META name mSuffix
  , range = Range (row p) (column p) (row p) (column p + l - 1)
  }

-- cond :: Token Text -> Bool
-- cond t = t `notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN]

pREGEX :: Position -> Parser Node
pREGEX p = do
  _ <- string cSLASH
  -- r <- manyTill printChar (string cSLASH)
  let cond s = s `notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN]
  r <- takeWhile1P Nothing cond :: Parser Text

  suffix <- pack <$> many alphaNumChar
  let l = 1 + T.length r + 1 + T.length suffix
  return initNode {
    nodeToken = REGEX r suffix
  , range = (rangeFrom p) {endColumn = column p + l - 1}
  , nodes = []
  }

-- | works for single-line terms
nodeLength :: Node -> Int
nodeLength n = l
  where
    Range _ column1 _ column2 = range n
    l = column2 - column1 + 1

pEOL :: Position -> Parser Node
pEOL p = do
  eols <- eol
  indents <- fmap length (many (string cINDENT))
  let node = initNode
  return initNode {
    nodeToken = INDENT indents
  , range = (rangeFrom p) {endRow = startRow (range node), endColumn = indents * 2 - 1}
  }

positionAfter :: Node -> Position
positionAfter n = Position r (c+1)
  where
    Range _ _ r c = range n

pBYTES :: Position -> Parser Node
pBYTES p = do
  _ <- pEMPTY_BYTES p
  return initNode
  where
    p1 p = pEMPTY_BYTES p
    p2 p = do
      byte <- pBYTE p
      _ <- pTerminal cMINUS MINUS (positionAfter byte)
      let l = nodeLength byte + 1
      return byte {range = (rangeFrom p) {endColumn = column p + l - 1}}




main :: IO ()
main = do
  code <- readFile "./app/code.eo"
  -- parseTest pScheme $ pack code
  putStrLn "\n"
  let p = Position 0 0
  parseTest (
    pLINE_BYTES p <|>
    pCOMMENT p <|>
    pMETA p <|>
    pEMPTY_BYTES p <|>
    pREGEX p <|>
    pEOL p
    ) (pack code)

