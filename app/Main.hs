{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad.Identity
import           Data.Char                  (digitToInt)
import qualified Data.List                  as DL
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (takeWhile1P), Parsec,
                                             SourcePos (SourcePos), choice,
                                             count, eof, getSourcePos, many,
                                             manyTill, option, parseTest, some,
                                             try, unPos)
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             hexDigitChar, letterChar,
                                             numberChar, printChar, string)
import           Text.Megaparsec.Char.Lexer (charLiteral, decimal, scientific,
                                             signed)
import           Text.Megaparsec.Debug      (dbg)
import           Text.Printf                (printf)

type Parser = Parsec Void Text

cARROW :: Text
cARROW = ">"

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
  | SeveralNode
  | JustNode
  | NothingNode
  deriving (Show)

data Position = Position
  { row    :: Int,
    column :: Int
  }

instance Show Position where
  show (Position r c) = printf "%d:%d" r c

data Node = Node
  { nodeToken :: TokenType,
    nodes     :: [Node],
    start     :: Position,
    end       :: Position
  }

tab :: String
tab = "|  "

type TabNumber = Int

printTree :: TabNumber -> Node -> String
printTree n Node {..} =
  DL.intercalate "" (replicate n tab)
    <> case nodeToken of
      JustNode -> printf "%s\n" (show nodeToken)
      SeveralNode -> printf "%s\n" (show nodeToken)
      NothingNode -> printf "%s\n" (show nodeToken)
      _ -> printf "%s [%s..%s]\n" (show nodeToken) (show start) (show end)
    <> foldl (\s a -> s <> printTree (n + 1) a) "" nodes

instance Show Node where
  show n = printTree 0 n

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
  guard (c `elem` ['0' .. '9'] <> [l1 .. l2])
  let c' = digitToInt c
  return c'

pHexDigitUpper :: Parser Integer
pHexDigitUpper = toInteger <$> pHexDigit 'A' 'F'

pHexDigitLower :: Parser Integer
pHexDigitLower = toInteger <$> pHexDigit 'a' 'f'

pEmpty :: Parser ()
pEmpty = return ()

hexToInt :: [Integer] -> Integer
hexToInt = foldl (\x acc -> (acc * 16) + x) 0

pTerminal :: Text -> TokenType -> Parser Node
pTerminal s t = do
  p1 <- getPos
  s <- string s
  p2 <- getPos
  return
    initNode
      { nodeToken = t,
        start = p1,
        end = p2
      }

listNode :: [Node] -> Node
listNode ns =
  initNode
    { nodeToken = SeveralNode,
      nodes = ns
    }

-- optionalNode p = do
--   p1 <- getPos
--   t <- optional (try p)
--   p2 <- getPos
--   let n = initNode {start = p1, end = p2}
--   let res =
--         case t of
--           Just s ->
--             n {
--               nodeToken = JustNode
--             , nodes = [s]
--             }
--           Nothing ->
--             n {
--               nodeToken = NothingNode
--             }
--   return res

maybeToNode :: Maybe Node -> Node
maybeToNode (Just n) =
  initNode
    { nodeToken = JustNode,
      nodes = [n]
    }
maybeToNode Nothing =
  initNode
    { nodeToken = NothingNode
    }

optionalNode :: Parser Node -> Parser Node
optionalNode p = do
  p1 <- getPos
  n <- maybeToNode <$> optional (try p)
  p2 <- getPos
  return n

pProgram :: Parser Node
pProgram = do
  p1 <- getPos
  l <- optionalNode pLicense
  m <- optionalNode pMetas
  o <- optionalNode pObjects
  _ <- eof
  p2 <- getPos
  return
    initNode
      { nodeToken = Program,
        start = p1,
        end = p2,
        nodes = [l, m, o]
      }

pLicense :: Parser Node
pLicense = do
  p1 <- getPos
  ms <- some (pCOMMENT <* eol)
  p2 <- getPos
  return
    initNode
      { nodeToken = License,
        start = p1,
        end = p2,
        nodes = ms
      }

pMetas :: Parser Node
pMetas = do
  p1 <- getPos
  ms <- some (pMETA <* eol)
  p2 <- getPos
  return
    initNode
      { nodeToken = Metas,
        start = p1,
        end = p2,
        nodes = ms
      }

pObjects :: Parser Node
pObjects = do
  p1 <- getPos
  let m = do
        cs <- listNode <$> many (pCOMMENT <* eol)
        obj <- pObject
        _ <- eol
        return (listNode [cs, obj])
  ms <- some m
  p2 <- getPos
  return
    initNode
      { nodeToken = Objects,
        start = p1,
        end = p2,
        nodes = ms
      }

pObject :: Parser Node
pObject = do
  p1 <- getPos
  comments <- debug "object:comments" $ listNode <$> many (pCOMMENT <* eol)
  a <-
      choice
        [ debug "object:abstraction" (try pAbstraction),
          debug "object:application" pApplication
        ]
  t <- debug "object:pTail" $ optionalNode pTail
  let g = do
        _ <- eol
        method <- pMethod
        h <- optionalNode pHtail
        suffix <- optionalNode pSuffix
        p <- optionalNode pTail
        return (listNode [method, h, suffix, p])
  s <- debug "object:after tail" $ listNode <$> many g
  p2 <- getPos
  return
    initNode
      { nodeToken = Object,
        start = p1,
        end = p2,
        nodes = [comments, a, t, s]
      }

debugFlag = True

-- debug :: (Text.Megaparsec.Stream.VisualStream s, Text.Megaparsec.Error.ShowErrorComponent e, Show a) => String -> Text.Megaparsec.MonadParsec e s m a -> Text.Megaparsec.MonadParsec e s m a
debug s p
  | debugFlag = dbg s p
  | otherwise = p


pAbstraction :: Parser Node
pAbstraction = do
  p1 <- getPos
  attrs <- debug "abstraction:attributes" pAttributes
  t <- debug "abstraction:optional" $
    optionalNode
      ( listNode <$> choice
          [ try $ do
              suff <- pSuffix
              o <- optionalNode (string cSPACE *> string cSLASH *> (pNAME <|> pTerminal cQUESTION QUESTION))
              return [suff, o],
            do
              htail <- pHtail
              return [htail]
          ]
      )
  p2 <- getPos
  return
    initNode
      { start = p1,
        end = p2,
        nodes = [attrs, t]
      }

pAttributes :: Parser Node
pAttributes = do
  p1 <- getPos
  _ <- string cLSQ
  let attrs = do
        a <- pAttribute
        as <- many (string cSPACE *> pAttribute)
        return (listNode (a : as))
  attrs' <- debug "attributes:attributes" $ optionalNode attrs
  _ <- string cRSQ
  p2 <- getPos
  return
    initNode
      { nodeToken = Attributes,
        start = p1,
        end = p2,
        nodes = [attrs']
      }

pAttribute :: Parser Node
pAttribute = pLabel

pLabel :: Parser Node
pLabel = do
  p1 <- getPos
  l <-
    debug "label" $ choice
      [ try $ (: []) <$> pTerminal cAT AT,
        do
          name <- pNAME
          cdots <- optionalNode (pTerminal cDOTS DOTS)
          return [name, cdots]
      ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Label,
        start = p1,
        end = p2,
        nodes = l
      }

pTail :: Parser Node
pTail = do
  p1 <- getPos
  e <- pEOL_INDENT
  objects <- listNode <$> some (pObject <* pEOL_INDENT)
  p2 <- getPos
  return
    initNode
      { nodeToken = Tail,
        start = p1,
        end = p2,
        nodes = [e, objects]
      }

pSuffix :: Parser Node
pSuffix = do
  p1 <- getPos
  label <- debug "suffix:label" $ string cSPACE *> string cARROW *> string cSPACE *> pLabel
  c <- debug "suffix:const" $ optionalNode (pTerminal cCONST CONST)
  p2 <- getPos
  return
    initNode
      { nodeToken = Suffix,
        start = p1,
        end = p2,
        nodes = [label, c]
      }

pMethod :: Parser Node
pMethod = do
  p1 <- getPos
  method <-
    string cDOT
      *> choice
        [ pNAME,
          pTerminal cRHO RHO,
          pTerminal cAT AT,
          pTerminal cVERTEX VERTEX
        ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Method,
        start = p1,
        end = p2,
        nodes = [method]
      }

pApplication :: Parser Node
pApplication = do
  p1 <- getPos
  nodes <- debug "application:nodes" $
    choice $ map try
      [ do
          h <- pHead
          ht <- optionalNode pHtail
          return [h, ht],
        do
          application <- pApplication
          method <- pMethod
          htail <- optionalNode pHtail
          return [application, method, htail],
        do
          application <- string cLB *> pApplication <* string cRB
          htail <- optionalNode pHtail
          return [application, htail],
        do
          application <- pApplication
          has <- pHas
          htail <- optionalNode pHtail
          return [application, has, htail],
        do
          application <- pApplication
          suffix <- pSuffix
          htail <- optionalNode pHtail
          return [application, suffix, htail]
      ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Application,
        start = p1,
        end = p2,
        nodes = nodes
      }

pHtail :: Parser Node
pHtail = do
  p1 <- getPos
  t <-
    some $
      listNode
        <$> choice
          [ try $ do
              app <- string cSPACE *> pApplication
              m <- pMethod
              return [app, m],
            try $ do
              app <- string cSPACE *> string cLB *> pApplication <* string cRB
              return [app],
            try $ do
              app <- string cSPACE *> pApplication
              has <- pHas
              return [app, has],
            try $ do
              app <- string cSPACE *> pApplication
              suff <- pSuffix
              return [app, suff],
            try $ do
              abstr <- string cSPACE *> pAbstraction
              return [abstr]
          ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Htail,
        start = p1,
        end = p2,
        nodes = t
      }

pHead :: Parser Node
pHead = do
  p1 <- getPos
  _ <- optional (string cDOTS)
  t <-
    choice
      [ pTerminal cROOT ROOT,
        pTerminal cAT AT,
        pTerminal cRHO RHO,
        pTerminal cXI XI,
        pTerminal cSIGMA SIGMA,
        pTerminal cSTAR STAR,
        pNAME <* optional (string cCOPY),
        pNAME <* string cDOT,
        pDATA
      ]
  p2 <- getPos
  return
    initNode
      { nodeToken = Head,
        start = p1,
        end = p2,
        nodes = [t]
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
        nodes = [d]
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
        nodes = byte : bytes,
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
        nodes = bytes
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
      lbs <- concat <$> many parser4
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
  t <- pack <$> (string "\"\"\"" *> eol *> manyTill charLiteral (string "\"\"\""))
  p2 <- getPos
  return
    initNode
      { nodeToken = TEXT t,
        start = p1,
        end = p2
      }

main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  -- parseTest pBYTES code
  parseTest (debug "object" pObject) code
