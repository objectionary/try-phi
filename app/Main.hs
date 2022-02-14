{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad              (guard)
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import qualified Data.List                  as DL
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text, head, null, pack, singleton,
                                             unpack)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (notFollowedBy, takeWhile1P),
                                             Parsec, ParsecT (..),
                                             SourcePos (SourcePos),
                                             Stream (Token, Tokens), choice,
                                             empty, getSourcePos, many,
                                             manyTill, noneOf, parseTest,
                                             runParser, runParserT, satisfy,
                                             some, takeWhileP, try, unPos,
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, crlf, eol,
                                             newline, printChar, string)

import           Text.Printf                (printf)
-- data PositionState =
--   PositionState {
--     row :: Int
--   , column :: Int
--   } deriving (Show)

-- type Parser = StateT Position (ParsecT Void Text Identity)
type Parser = Parsec Void Text

data Position =
  Position {
    row    :: Int
  , column :: Int
  }

instance Show Position where
  show (Position r c) = printf "%d:%d" r c


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
  | BYTES
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
  , start     :: Position
  , end       :: Position
  }


tab :: String
tab = "|  "

type TabNumber = Int

printTree :: TabNumber -> Node -> String
printTree n Node{..} =
  DL.intercalate "" (replicate n tab)
  <> printf "%s [%s..%s]\n" (show nodeToken) (show start) (show end)
  <> foldl (\s a -> s <> printTree (n + 1) a) "" nodes

instance Show Node where
  show n = printTree 0 n

initNode :: Node
initNode =
  Node {
    nodeToken = NONE
  , nodes = []
  , start = Position 0 0
  , end = Position 0 0
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

getPos :: Parser Position
getPos = do
  SourcePos f r c <- getSourcePos
  return (Position (unPos r) (unPos c))

inByteRange :: Char -> Bool
inByteRange c
  | '0' <= c && c <= '9' = True
  | 'A' <= c && c <= 'F' = True
  | otherwise = False

pBYTE :: Parser Node
pBYTE = do
  p1 <- getPos
  s1 <- alphaNumChar
  guard (inByteRange s1)
  s2 <- alphaNumChar
  guard (inByteRange s2)
  p2 <- getPos
  return initNode {
    nodeToken = BYTE (pack [s1, s2])
  , start = p1
  , end = p2
  }

pEMPTY_BYTES :: Parser Text
pEMPTY_BYTES = string cEMPTY_BYTES

pLINE_BYTES :: Parser Node
pLINE_BYTES = do
  p1 <- getPos
  byte <- pBYTE
  bytes <- some (string cMINUS *> pBYTE)
  p2 <- getPos
  return initNode {
    nodeToken = LINE_BYTES
  , nodes = byte : bytes
  , start = p1
  , end = p2
  }

pBB :: Parser Node
pBB = do
  p1 <- getPos
  b1 <- pBYTE
  _ <- string cMINUS
  b2 <- pBYTE
  p2 <- getPos
  return initNode {
    nodeToken = Some ""
  , nodes = [b1,b2]
  , start = p1
  , end = p2
  }

pCOMMENT :: Parser Node
pCOMMENT = do
  p1 <- getPos
  _ <- string cHASH
  content <- pack <$> many printChar
  p2 <- getPos
  return initNode {
    nodeToken = COMMENT content
  , start = p1
  , end = p2
  }

pOptionalString :: Parser String -> Parser Text
pOptionalString p = do
  s <- optional (try p)
  let t = maybe cEMPTY_TEXT pack s
  return t

pMETA :: Parser Node
pMETA = do
  p1 <- getPos
  _ <- string cPLUS
  name <- pack <$> many alphaNumChar
  suffix <- pOptionalString (char ' ' *> many printChar)
  p2 <- getPos
  return initNode {
    nodeToken = META name suffix
  , start = p1
  , end = p2
  }

pREGEX :: Parser Node
pREGEX = do
  p1 <- getPos
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  p2 <- getPos
  return initNode {
    nodeToken = REGEX r suffix
  , start = p1
  , end = p2
  }

pEOL_INDENT :: Parser Node
pEOL_INDENT = do
  p1 <- getPos
  _ <- eol
  indents <- T.concat <$> many (string cINDENT)
  p2 <- getPos
  let nIndents = T.length indents `div` 2
  return initNode {
    nodeToken = INDENT nIndents
  , start = p1
  , end = p2
  }

pBYTES :: Parser Node
pBYTES = do
  p1 <- getPos
  bytes <- choice (map try [parser1, parser3, parser2])
  p2 <- getPos
  return initNode {
    nodeToken = BYTES
  , start = p1
  , end = p2
  , nodes = bytes
  }
  where
    parser1 = do
      _ <- pEMPTY_BYTES
      return []
    parser2 = do
      byte <- pBYTE
      _ <- string cMINUS
      return [byte]
    parser4 = do
      _ <- string cMINUS
      e <- pEOL_INDENT
      lb <- pLINE_BYTES
      return [e, lb]
    parser3 = do
      lb <- pLINE_BYTES
      lbs <- concat <$> many parser4
      return (lb:lbs)


-- pImplemented :: StateT Position (ParsecT Void Text Identity) [Node]
pImplemented :: ParsecT Void Text Identity [Node]
pImplemented = many (choice $ map try [pCOMMENT, pMETA, pEOL_INDENT, pREGEX, pLINE_BYTES, pBB])

pTest :: Parser Node
pTest = pLINE_BYTES

main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  parseTest pImplemented code

