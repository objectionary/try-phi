{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import           Control.Applicative  (Alternative ((<|>)), optional)
import           Control.Monad        (guard)
import qualified Data.List            as DL
import           Data.Text            (Text, head, null, pack, singleton,
                                       unpack)
import qualified Data.Text            as T
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (notFollowedBy, takeWhile1P),
                                       Parsec, Stream (Token, Tokens), choice,
                                       many, manyTill, noneOf, parseTest,
                                       satisfy, some, takeWhileP, try, (<|>))
import           Text.Megaparsec.Char (alphaNumChar, char, crlf, eol, newline,
                                       printChar, string)
import Data.Maybe (fromMaybe)



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
  | META Text Text
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
  | TAB
  | TEXT Text
  | TEXT_MARK
  | UNTAB
  | VERTEX
  | XI
  | NONE
  | Some Text
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

initNode :: Node
initNode =
  Node {
    nodeToken = NONE
  , nodes = []
  , range = Range 0 0 0 (-1)
  }

pTerminal :: Text -> TokenType -> Node -> Parser Node
pTerminal s t node = do
  n <- nodeAfter node <$> string s
  return n {
    nodeToken = t
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
cEMPTY_BYTES :: Text
cEMPTY_BYTES = "--"
cEMPTY_TEXT :: Text
cEMPTY_TEXT = ""

inByteRange :: Char -> Bool
inByteRange c
  | '0' <= c && c <= '9' = True
  | 'A' <= c && c <= 'F' = True
  | otherwise = False

pBYTE :: Node -> Parser Node
pBYTE p = do
  s1 <- alphaNumChar
  guard (inByteRange s1)
  s2 <- alphaNumChar
  guard (inByteRange s2)
  let byte = pack [s1, s2]
  return (nodeAfter p byte) {
    nodeToken = BYTE byte
  }

-- takeP eats many tokens

pEMPTY_BYTES :: Node -> Parser Node
pEMPTY_BYTES = pTerminal cEMPTY_BYTES EMPTY_BYTES


pLINE_BYTES :: Node -> Parser Node
pLINE_BYTES p = do
    byte <- pBYTE p
    byteNodes <- some (string cMINUS *> pBYTE p)
    let indexed = DL.scanl correctPosition byte byteNodes
    return initNode {
      nodeToken = LINE_BYTES
    , range = concatRangesFrom byte (DL.last indexed)
    , nodes = indexed
    }
    where
      correctPosition n a = correct a
        where
          aStartColumn = endColumn (range n) + 2
          aEndColumn = aStartColumn + 1
          correct x = x {range = (range x) {startColumn = aStartColumn, endColumn = aEndColumn}}

pCOMMENT :: Node -> Parser Node
pCOMMENT p = do
  h <- nodeAfter p <$> string cHASH
  content <- nodeAfter h <$> (pack <$> (many printChar :: Parser String))
  return initNode {
    nodeToken = COMMENT (nodeText content)
  , range = concatRangesFrom h content
  }

pOptionalString :: Parser String -> Parser Text
pOptionalString p = do
  s <- optional (try p)
  let t = maybe cEMPTY_TEXT pack s
  return t

pMETA :: Node -> Parser Node
pMETA p = do
  plus <- nodeAfter p <$> string cPLUS
  name <- nodeAfter plus <$> (pack <$> many alphaNumChar)
  suffix <- nodeAfter name <$> pOptionalString ((:) <$> char ' ' <*> many printChar)
  return initNode {
    nodeToken = META (nodeText name) (nodeText suffix)
  , range = concatRangesFrom plus suffix
  -- , nodes = [plus, name, suffix]
  }

nodeAfter :: Node -> Text -> Node
nodeAfter (Node _ _ (Range _ _ r c)) t =
  initNode {
    nodeToken = Some t
  , range = Range r (c + 1) r (c + T.length t)
  }

nodeText :: Node -> Text
nodeText (Node t _ _) =
  case t of
    Some s -> s
    _ -> T.empty

pREGEX :: Node -> Parser Node
pREGEX p = do
  slash1 <- pTerminal cSLASH SLASH p
  r <- nodeAfter slash1 <$> takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  slash2 <- pTerminal cSLASH SLASH r
  suffix <- nodeAfter slash2 <$> (pack <$> many alphaNumChar)
  return initNode {
    nodeToken = REGEX (nodeText r) (nodeText suffix)
  , range = concatRangesFrom slash1 suffix
  }

pEOL_INDENT :: Node -> Parser Node
pEOL_INDENT p = do
  eols <- nodeAfter p <$> eol
  let rng@(Range _ _ r _) = range eols
  let newRow = eols {range = rng {endRow = r + 1, endColumn = -1}}
  indents <- nodeAfter newRow <$> (foldl (<>) T.empty <$> many (string cINDENT))
  let nIndents = T.length (nodeText indents) `div` 2
  return indents {
    nodeToken = INDENT nIndents
  }

concatRanges :: Range -> Range -> Range
concatRanges (Range r1 c1 _ _) (Range _ _ r2 c2) = Range r1 c1 r2 c2

concatRangesFrom :: Node -> Node -> Range
concatRangesFrom (Node _ _ r1) (Node _ _ r2) = concatRanges r1 r2

pBYTES :: Node -> Parser Node
pBYTES p = do
  _ <- pEMPTY_BYTES p
  return initNode
  where
    p1 p = do
      emp <- pEMPTY_BYTES p
      return [emp]
    p2 p = do
      byte <- pBYTE p
      minus <- pTerminal cMINUS MINUS byte
      return [byte, minus]
    p4 p = do
      m <- pTerminal cMINUS MINUS p
      e <- pEOL_INDENT m
      lb <- pLINE_BYTES e
      return [e, lb]
    p3 p = do
      lb <- pLINE_BYTES p
      lbs <- concat <$> many (p4 p)
      -- IDK how to correct indexation
      return lbs



main :: IO ()
main = do
  code <- readFile "./app/code.eo"
  -- parseTest pScheme $ pack code
  putStrLn "\n"
  let p = initNode
  parseTest (many $
    try (pLINE_BYTES p) <|>
    try (pCOMMENT p) <|>
    try (pMETA p) <|>
    try (pEMPTY_BYTES p) <|>
    try (pREGEX p) <|>
    try (pEOL_INDENT p)
    ) (pack code)

