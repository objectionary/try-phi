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
                                             Stream (Token, Tokens), choice,
                                             empty, many, manyTill, noneOf,
                                             parseTest, runParser, runParserT,
                                             satisfy, some, takeWhileP, try,
                                             (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, crlf, eol,
                                             newline, printChar, string)

import           Text.Printf                (printf)
-- data PositionState =
--   PositionState {
--     row :: Int
--   , column :: Int
--   } deriving (Show)

type Parser = StateT Position (ParsecT Void Text Identity)
-- type Parser = ParsecT Void Text State

data Position =
  Position {
    row    :: Int
  , column :: Int
  }

instance Show Position where
  show (Position r c) = printf "%d:%d" (r+1) (c+1)


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
  , end = Position 0 (-1)
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

next :: Parser ()
next = advance 1

advance :: Int -> Parser ()
advance n = do
  p <- get
  put (p {column = column p + n})
  return ()

-- | parses text, advances position
pText :: Text -> Parser Text
pText s = do
  t <- string s
  advance (T.length s-1)
  return t

inByteRange :: Char -> Bool
inByteRange c
  | '0' <= c && c <= '9' = True
  | 'A' <= c && c <= 'F' = True
  | otherwise = False

pBYTE :: Parser Node
pBYTE = do
  next
  p1 <- get
  s1 <- alphaNumChar
  guard (inByteRange s1)
  s2 <- alphaNumChar
  guard (inByteRange s2)
  next
  p2 <- get
  return initNode {
    nodeToken = BYTE (pack [s1, s2])
  , start = p1
  , end = p2
  }

pEMPTY_BYTES :: Parser Text
pEMPTY_BYTES = pText cEMPTY_BYTES

pLINE_BYTES :: Parser Node
pLINE_BYTES = do
  next
  p1 <- get
  byte <- pBYTE
  bytes <- some (pText cMINUS *> pBYTE)
  p2 <- get
  return initNode {
    nodeToken = LINE_BYTES
  , nodes = byte : bytes
  , start = p1
  , end = p2
  }

pCOMMENT :: Parser Node
pCOMMENT = do
  next
  p1 <- get
  h <- string cHASH
  advance (T.length h)
  content <- pack <$> many printChar
  advance (T.length content - 1)
  p2 <- get
  return initNode {
    nodeToken = COMMENT content
  , start = p1
  , end = p2
  }

pOptionalString :: Parser String -> Parser Text
pOptionalString p = do
  next
  s <- optional (try p)
  advance (length s - 1)
  let t = maybe cEMPTY_TEXT pack s
  return t

pMETA :: Parser Node
pMETA = do
  next
  p1 <- get
  _ <- string cPLUS
  next
  name <- pack <$> many alphaNumChar
  advance (T.length name)
  suffix <- pOptionalString (char ' ' *> many printChar)
  advance (T.length suffix)
  p2 <- get
  return initNode {
    nodeToken = META name suffix
  , start = p1
  , end = p2
  }

pREGEX :: Parser Node
pREGEX = do
  next
  p1 <- get
  _ <- string cSLASH
  next
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  advance (T.length r)
  _ <- string cSLASH
  next
  suffix <- pack <$> many alphaNumChar
  advance (T.length suffix - 1)
  p2 <- get
  return initNode {
    nodeToken = REGEX r suffix
  , start = p1
  , end = p2
  }

nextLine :: Parser ()
nextLine = do
  p <- get
  put p {row = row p + 1, column = -1}
  return ()

pEOL_INDENT :: Parser Node
pEOL_INDENT = do
  next
  p1 <- get
  eols <- eol
  advance (T.length eols)
  nextLine
  indents <- T.concat <$> many (pText cINDENT)
  p2 <- get
  let nIndents = T.length indents `div` 2
  return initNode {
    nodeToken = INDENT nIndents
  , start = p1
  , end = p2
  }

pBYTES :: Parser Node
pBYTES = do
  next
  p1 <- get
  bytes <- choice (map try [parser1, parser3, parser2])
  p2 <- get
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
      _ <- pText cMINUS
      return [byte]
    parser4 = do
      _ <- pText cMINUS
      e <- pEOL_INDENT
      lb <- pLINE_BYTES
      return [e, lb]
    parser3 = do
      lb <- pLINE_BYTES
      lbs <- concat <$> many parser4
      return (lb:lbs)

main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  -- let p            = runStateT parser "initial"
  --     Right (a, s) = runParser p "" ""
  -- putStrLn ("Result:      " ++ show a)
  -- putStrLn ("Final state: " ++ show s)

  let p = runStateT (many (choice $ map try [pCOMMENT, pMETA, pEOL_INDENT, pREGEX, pLINE_BYTES, pBYTE])) (Position 0 (-1))
      a = runParser p "" code
  putStrLn ("Result:      " ++ show a)

