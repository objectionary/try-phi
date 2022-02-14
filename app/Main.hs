{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad (guard)
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Char (digitToInt)
import qualified Data.List as DL
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text, head, null, pack, unpack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (notFollowedBy, takeWhile1P),
    Parsec,
    ParsecT (..),
    SourcePos (SourcePos),
    Stream (Token, Tokens),
    choice,
    count,
    empty,
    getInput,
    getSourcePos,
    many,
    manyTill,
    noneOf,
    option,
    parse,
    parseTest,
    runParser,
    runParserT,
    satisfy,
    setInput,
    some,
    takeWhileP,
    try,
    unPos,
    (<|>), ErrorItem (Label)
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    crlf,
    eol,
    hexDigitChar,
    letterChar,
    newline,
    numberChar,
    printChar,
    space,
    string,
  )
import Text.Megaparsec.Char.Lexer
  ( charLiteral,
    decimal,
    float,
    hexadecimal,
    scientific,
    signed,
  )
import Text.Printf (printf)

type Parser = Parsec Void Text

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

cTRUE :: Text
cTRUE = "TRUE"

cFALSE :: Text
cFALSE = "FALSE"

data TokenType
  = -- Non-terminals
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
  | -- Terminals (regex-recognizable)
    ARROW
  | AT
  | BOOL Bool
  | BYTE Integer
  | BYTES
  | CHAR Char
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
  | FLOAT Scientific
  | HASH
  | HEX Integer
  | INDENT Int
  | INT Integer
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
  | STRING Text
  | TAB
  | TEXT Text
  | TEXT_MARK
  | UNTAB
  | VERTEX
  | XI
  | NONE
  | Some
  | Many
  | Group
  deriving (Show)

data Position = Position
  { row :: Int,
    column :: Int
  }

instance Show Position where
  show (Position r c) = printf "%d:%d" r c

data Node = Node
  { nodeToken :: TokenType,
    nodes :: [Maybe Node],
    start :: Position,
    end :: Position
  }

tab :: String
tab = "|  "

type TabNumber = Int

printTree :: TabNumber -> Maybe Node -> String
printTree n (Just Node {..}) =
  DL.intercalate "" (replicate n tab)
    <> printf "%s [%s..%s]\n" (show nodeToken) (show start) (show end)
    <> foldl (\s a -> s <> printTree (n + 1) a) "" nodes
printTree _ Nothing = ""

instance Show Node where
  show n = printTree 0 (Just n)

initNode :: Node
initNode =
  Node
    { nodeToken = NONE,
      nodes = [],
      start = Position 0 0,
      end = Position 0 0
    }

getPos :: Parser Position
getPos = do
  SourcePos _ r c <- getSourcePos
  return (Position (unPos r) (unPos c))

pHexDigit :: Char -> Char -> Parser Int
pHexDigit l1 l2 = do
  c <- hexDigitChar
  guard (c `elem` (['0' .. '9'] <> [l1 .. l2]))
  let c' = digitToInt c
  return c'

pHexDigitUpper :: Parser Integer
pHexDigitUpper = toInteger <$> pHexDigit 'A' 'F'

pHexDigitLower :: Parser Integer
pHexDigitLower = toInteger <$> pHexDigit 'a' 'f'

pBYTE :: Parser Node
pBYTE = do
  p1 <- getPos
  b <- hexToInt <$> count 2 pHexDigitUpper
  p2 <- getPos
  return
    initNode
      { nodeToken = BYTE b,
        start = p1,
        end = p2
      }

pLINE_BYTES :: Parser Node
pLINE_BYTES = do
  p1 <- getPos
  byte <- pBYTE
  bytes <- some (string cMINUS *> pBYTE)
  p2 <- getPos
  return
    initNode
      { nodeToken = LINE_BYTES,
        nodes = Just <$> (byte : bytes),
        start = p1,
        end = p2
      }

pCOMMENT :: Parser Node
pCOMMENT = do
  p1 <- getPos
  _ <- string cHASH
  content <- pack <$> many printChar
  p2 <- getPos
  return
    initNode
      { nodeToken = COMMENT content,
        start = p1,
        end = p2
      }

pMETA :: Parser Node
pMETA = do
  p1 <- getPos
  _ <- string cPLUS
  name <- pack <$> many alphaNumChar
  suffix <- pack <$> option "" (string cSPACE *> many printChar)
  p2 <- getPos
  return
    initNode
      { nodeToken = META name suffix,
        start = p1,
        end = p2
      }

pREGEX :: Parser Node
pREGEX = do
  p1 <- getPos
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  p2 <- getPos
  return
    initNode
      { nodeToken = REGEX r suffix,
        start = p1,
        end = p2
      }

pEOL_INDENT :: Parser Node
pEOL_INDENT = do
  p1 <- getPos
  _ <- eol
  indents <- T.concat <$> many (string cINDENT)
  p2 <- getPos
  let nIndents = T.length indents `div` 2
  return
    initNode
      { nodeToken = INDENT nIndents,
        start = p1,
        end = p2
      }

pBYTES :: Parser Node
pBYTES = do
  p1 <- getPos
  bytes <-
    choice
      ( map
          try
          [ parser1,
            parser3,
            parser2
          ]
      )
  p2 <- getPos
  return
    initNode
      { nodeToken = BYTES,
        start = p1,
        end = p2,
        nodes = Just <$> bytes
      }
  where
    parser1 = do
      _ <- string cEMPTY_BYTES
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
      lbs <- concat <$> many (try parser4)
      return (lb : lbs)

pBOOL :: Parser Node
pBOOL = do
  p1 <- getPos
  b <-
    choice
      [ BOOL True <$ string cTRUE,
        BOOL False <$ string cFALSE
      ]
  p2 <- getPos
  return
    initNode
      { nodeToken = b,
        start = p1,
        end = p2
      }

-- | slightly differs from grammar: doesn't allow u Byte Byte
pCHAR :: Parser Node
pCHAR = do
  p1 <- getPos
  c <- char '\'' *> charLiteral <* char '\''
  p2 <- getPos
  return
    initNode
      { nodeToken = CHAR c,
        start = p1,
        end = p2
      }

-- | slightly differs from grammar: doesn't allow u Byte Byte
pSTRING :: Parser Node
pSTRING = do
  p1 <- getPos
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  p2 <- getPos
  return
    initNode
      { nodeToken = STRING s,
        start = p1,
        end = p2
      }

pEmpty :: Parser ()
pEmpty = return ()

pINT :: Parser Node
pINT = do
  p1 <- getPos
  s <- signed pEmpty decimal
  p2 <- getPos
  return
    initNode
      { nodeToken = INT s,
        start = p1,
        end = p2
      }

pFLOAT :: Parser Node
pFLOAT = do
  p1 <- getPos
  f <- signed pEmpty scientific
  p2 <- getPos
  return
    initNode
      { nodeToken = FLOAT f,
        start = p1,
        end = p2
      }

hexToInt :: [Integer] -> Integer
hexToInt xs = foldl (\x acc -> (acc * 16) + x) 0 xs

pHEX :: Parser Node
pHEX = do
  p1 <- getPos
  s <- hexToInt <$> (string "0x" *> some pHexDigitLower)
  p2 <- getPos
  return
    initNode
      { nodeToken = HEX s,
        start = p1,
        end = p2
      }

pNAME :: Parser Node
pNAME = do
  p1 <- getPos
  l1 <- alphaNumChar
  guard (l1 `elem` ['a' .. 'z'])
  l2 <- many (letterChar <|> numberChar <|> char '_' <|> char '-')
  p2 <- getPos
  return
    initNode
      { nodeToken = NAME (pack (l1 : l2)),
        start = p1,
        end = p2
      }

-- IDK maybe need to allow indentation after eol
pTEXT :: Parser Node
pTEXT = do
  p1 <- getPos
  t <- pack <$> (string "\"\"\"" *> manyTill (charLiteral <|> newline <|> crlf *> newline) (string "\"\"\""))
  p2 <- getPos
  return
    initNode
      { nodeToken = TEXT t,
        start = p1,
        end = p2
      }

pDATA :: Parser Node
pDATA = do
  p1 <- getPos
  d <-
    choice
      [ pBYTES,
        pBOOL,
        pTEXT,
        pSTRING,
        pINT,
        pFLOAT,
        pHEX,
        pCHAR,
        pREGEX
      ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Data,
        start = p1,
        end = p2,
        nodes = [Just d]
      }

pHas :: Parser Node
pHas = do
  p1 <- getPos
  _ <- string cCOLON
  n <- pNAME
  p2 <- getPos
  return
    initNode
      { nodeToken = Has,
        start = p1,
        end = p2
      }

pHead :: Parser Node
pHead = do
  _ <- optional (string cDOTS)
  t <-
    choice
      [ [] <$ string cROOT,
        [] <$ string cAT,
        [] <$ string cRHO,
        [] <$ string cXI,
        [] <$ string cSIGMA,
        [] <$ string cSTAR,
        (: []) <$> (pNAME <* optional (string cCOPY)),
        (: []) <$> pNAME <* string cDOT,
        (: []) <$> pDATA
      ]
  return initNode

pApplication :: Parser Node
pApplication = do
  p1 <- getPos
  nodes <-
    choice
      [ do
          head <- pHead
          htail <- optional pHtail
          return [Just head, htail]
      , do
          application <- pApplication
          method <- pMethod
          htail <- optional pHtail
          return [Just application, Just method, htail]
      , do
          application <- string cLB *> pApplication <* string cRB
          htail <- optional pHtail
          return [Just application, htail]
      , do
          application <- pApplication
          has <- pHas
          htail <- optional pHtail
          return [Just application, Just has, htail]
      , do
          application <- pApplication
          suffix <- pSuffix
          htail <- optional pHtail
          return [Just application, Just suffix, htail]
      ]
  p2 <- getPos
  return
    initNode
      { start = p1,
        end = p2,
        nodes = nodes
      }

pTerminal :: Text -> TokenType -> Parser Node
pTerminal s t = do
  p1 <- getPos
  s <- string s
  p2 <- getPos
  return initNode {
    nodeToken = t
  , start = p1
  , end = p2
  }


pMethod :: Parser Node
pMethod = do
  p1 <- getPos
  method <- string cDOT *> choice [
      pNAME
    , pTerminal cRHO RHO
    , pTerminal cAT AT
    , pTerminal cVERTEX VERTEX
    ]
  p2 <- getPos
  return initNode {
    start = p1
  , end = p2
  }

-- listToNode :: TokenType -> ParsecT Void Text Identity [Node] -> Parser Node
-- listToNode t n = do 
--   ns <- n
--   let mns = Just <$> ns
--   e <- listMaybeToNode t mns
--   return e

-- listMaybeToNode :: TokenType -> ParsecT Void Text Identity [Maybe Node] -> Parser Node
-- listMaybeToNode t n = return initNode {
--   nodeToken = t
-- , nodes = n
-- }

pObject :: Parser Node
pObject = do
  p1 <- getPos
  -- comments <- listToNode Many <$> many (pCOMMENT <* eol)
  a <- choice [
      pAbstraction
    , pApplication
    ]
  t <- optional pTail
  let g = do
        _ <- eol
        method <- pMethod
        h <- optional pHtail
        suffix <- optional pSuffix
        p <- optional pTail
        return [Just method, h, suffix, p]
  -- s <- listMaybeToNode Group g
  p2 <- getPos
  return initNode {
    start = p1
  , end = p2
  }


pTail :: Parser Node
pTail = do
  p1 <- getPos
  objects <- eol *> pEOL_INDENT *> some (pObject <* pEOL_INDENT)
  p2 <- getPos
  return initNode {
    start = p1
  , end = p2
  , nodes = Just <$> objects
  }

pAttributes :: Parser Node
pAttributes = do
  p1 <- getPos
  _ <- string cLSQ
  attrs <- (:) <$> pAttribute <*> many (string cSPACE *> pAttribute)
  _ <- string cRSQ
  p2 <- getPos
  return initNode {
    nodeToken = Attributes
  , start = p1
  , end = p2
  , nodes = Just <$> attrs
  }

pAttribute :: Parser Node
pAttribute = pLabel

pLabel :: Parser Node
pLabel = do
  p1 <- getPos
  l <- choice [
      pTerminal cAT AT
    , pNAME <* optional (string cDOTS)
    ]
  p2 <- getPos
  return initNode {
    start = p1
  , end = p2
  , nodes = [Just l]
  }

pSuffix :: Parser Node
pSuffix = do
  p1 <- getPos
  label <- string cSPACE *> string cARROW *> string cSPACE *> pLabel <* optional (string cCONST)
  p2 <- getPos
  return initNode {
    nodeToken = Suffix
  , start = p1
  , end = p2
  , nodes = [Just label]
  }

-- pObject :: Parser Node
-- pObject = do 


pAbstraction :: Parser Node
pAbstraction = do
  p1 <- getPos
  attrs <- pAttributes
  t <- optional (choice [
      do
        suff <- pSuffix
        o <- optional (string cSPACE *> string cSLASH *> (pNAME <|> pTerminal cQUESTION QUESTION))
        return initNode {
          nodes = [Just suff, o]
        }
    , do
        htail <- pHtail
        return initNode {
          nodes = [Just htail]
        }
    ])
  p2 <- getPos
  return initNode {
    start = p1
  , end = p2
  , nodes = [Just attrs, t]
  }

pHtail :: Parser Node
pHtail = do
  p1 <- getPos
  t <-
    some $
      choice
        [ do
            app <- string cSPACE *> pApplication
            m <- pMethod
            return [app, m],
          do
            app <- string cSPACE *> string cLB *> pApplication <* string cRB
            return [app],
          do
            app <- string cSPACE *> pApplication
            has <- pHas
            return [app, has],
          do
            app <- string cSPACE *> pApplication
            suff <- pSuffix
            return [app, suff],
          do
            abstr <- string cSPACE *> pAbstraction
            return [abstr]
        ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Htail,
        start = p1,
        end = p2
      }

pImplemented :: ParsecT Void Text Identity [Node]
pImplemented =
  many
    ( choice $
        map
          try
          [ pDATA,
            pCOMMENT,
            pMETA,
            pEOL_INDENT,
            pNAME,
            pHas,
            pHead
          ]
    )

main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  -- parseTest pBYTES code
  parseTest pImplemented code
