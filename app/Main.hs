{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Applicative        (Alternative ((<|>)))
import           Control.Monad              (guard)
import           Data.Text                  (Text)
import           Data.Text                  (pack)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (Parsec, parseTest, choice, Stream (Tokens))
import           Text.Megaparsec.Char       (string, space1, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L



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
  | BOOL String
  | BYTE String
  | BYTES String
  | CHAR String
  | COLON
  | COMMENT String
  | CONST
  | COPY
  | DOT
  | DOTS
  | EMPTY_BYTES
  | EOF
  | EOL String
  | ESCAPE_SEQUENCE String
  | EXPONENT String
  | FLOAT String
  | HASH
  | HEX
  | INDENT
  | INT String
  | LB
  | LINEBREAK String
  | LINE_BYTES String
  | LSQ
  | META String
  | MINUS
  | NAME String
  | PLUS
  | QUESTION
  | RB
  | REGEX String
  | RHO
  | ROOT
  | RSQ
  | SIGMA
  | SLASH
  | SPACE
  | STAR
  | STRING String
  | TAB
  | TEXT String
  | TEXT_MARK
  | UNTAB
  | VERTEX
  | XI
  | NONE

data Node =
  Node {
    nodeToken :: TokenType
  , nodes     :: [Node]
  , range     :: Range
  }

data Range =
  Range {
    startRow    :: Int
  , startColumn :: Int
  , endRow      :: Int
  , endColumn   :: Int
  }

data Position =
  Position {
    currentRow :: Int
  , currentColumn :: Int
  }

initNode =
  Node {
    nodeToken = NONE
  , nodes = []
  , range = Range 0 0 0 0
  }

-- will later be split

pTerminal :: Tokens Text -> TokenType -> Position -> Parser Node
pTerminal s t p = do
  c <- string s
  let Position row column = p
  return initNode {
    nodeToken = t
  , range = Range row column row (column + T.length c - 1)
  }

pARROW :: Position -> Parser Node
pARROW = pTerminal "<" ARROW
pAT :: Position -> Parser Node
pAT = pTerminal "@" AT
pCOLON :: Position -> Parser Node
pCOLON = pTerminal ":" COLON
pCONST :: Position -> Parser Node
pCONST = pTerminal "!" CONST
pCOPY :: Position -> Parser Node
pCOPY = pTerminal "\'" COPY
pDOT :: Position -> Parser Node
pDOT = pTerminal "." DOT
pHASH :: Position -> Parser Node
pHASH = pTerminal "#" HASH
pINDENT :: Position -> Parser Node
pINDENT = pTerminal "" INDENT
pLB :: Position -> Parser Node
pLB = pTerminal "(" LB
pLSQ :: Position -> Parser Node
pLSQ = pTerminal "[" LSQ
pMINUS :: Position -> Parser Node
pMINUS = pTerminal "-" MINUS
pPLUS :: Position -> Parser Node
pPLUS = pTerminal "+" PLUS
pQUESTION :: Position -> Parser Node
pQUESTION = pTerminal "?" QUESTION
pRB :: Position -> Parser Node
pRB = pTerminal ")" RB
pRHO :: Position -> Parser Node
pRHO = pTerminal "^" RHO
pROOT :: Position -> Parser Node
pROOT = pTerminal "Q" ROOT
pRSQ :: Position -> Parser Node
pRSQ = pTerminal "]" RSQ
pSIGMA :: Position -> Parser Node
pSIGMA = pTerminal "&" SIGMA
pSLASH :: Position -> Parser Node
pSLASH = pTerminal "/" SLASH
pSPACE :: Position -> Parser Node
pSPACE = pTerminal " " SPACE
pSTAR :: Position -> Parser Node
pSTAR = pTerminal "*" STAR
pVERTEX :: Position -> Parser Node
pVERTEX = pTerminal "<" VERTEX
pXI :: Position -> Parser Node
pXI = pTerminal "$" XI
pTEXT_MARK :: Position -> Parser Node
pTEXT_MARK = pTerminal "\"\"\"" TEXT_MARK
pDOTS :: Position -> Parser Node
pDOTS = pTerminal "..." DOTS

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
    nodeToken = BYTE [s1, s2]
  , range = Range row column row (column + 1)
  }

-- takeP eats many tokens

pEMPTY_BYTES :: Position -> Parser Node
pEMPTY_BYTES = pTerminal "--" EMPTY_BYTES

-- pLINE_BYTES :: Position -> Parser Node 
-- pLINE_BYTES p = do
--   s1 <- pBYTE 
--   s2 <- many pBYTE
  

main :: IO ()
main = do
  code <- readFile "./app/code.eo"
  parseTest pScheme $ pack code
