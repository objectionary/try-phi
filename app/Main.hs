{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

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
                                             manyTill, parseTest, some, try,
                                             unPos, (<?>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             hexDigitChar, letterChar,
                                             lowerChar, numberChar, printChar,
                                             string, newline, crlf)
import           Text.Megaparsec.Char.Lexer (charLiteral, decimal, scientific,
                                             signed)
import           Text.Megaparsec.Debug      (dbg)
import qualified Text.Megaparsec.Error
import           Text.Megaparsec.Internal   (ParsecT)
import qualified Text.Megaparsec.Stream
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
  | Application1
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
  | META
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
  | ListNode
  | JustNode
  | NothingNode
  | TextNode Text
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
      -- ListNode -> printf "%s\n" (show nodeToken)
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
  void (string s)
  p2 <- getPos
  return $ Node t [] p1 p2

listNode :: Parser [Node] -> Parser Node
listNode p = do
  ns <- try p
  return initNode
    { nodeToken = ListNode,
      nodes = ns
    }

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

-- IDK optionalNode $ listNode may always be NodeJust?
-- can listNode fail?

optionalNode :: Parser Node -> Parser Node
optionalNode p = do
  p1 <- getPos
  n <- maybeToNode <$> optional (try p)
  p2 <- getPos
  return n {start = p1, end = p2}

textNode :: Parser Text -> Parser Node
textNode txt = do
  p1 <- getPos
  t <- try txt
  p2 <- getPos
  return
    initNode
      { nodeToken = TextNode t,
        start = p1,
        end = p2
      }

debugFlag :: Bool
debugFlag = True

data DebugMode = On | Off

debug :: (Text.Megaparsec.Stream.VisualStream s, Text.Megaparsec.Error.ShowErrorComponent e, Show a) => String -> ParsecT e s m a -> ParsecT e s m a
debug label parser
  | debugFlag = dbg label parser
  | otherwise = parser <?> label

manyTry :: MonadParsec e s m => m a -> m [a]
manyTry p = try $ many (try p)

someTry :: MonadParsec e s m => m a -> m [a]
someTry p = try $ some (try p)

choiceTry :: MonadParsec e s m => [m a] -> m a
choiceTry p = try $ choice (map try p)

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************

pProgram :: Parser Node
pProgram = do
  p1 <- getPos
  enter "program"
  l <- optionalNode (debug "program:license" pLicense)
  m <- optionalNode (debug "program:metas" pMetas)
  o <- debug "program:objects" pObjects
  -- _ <- debug "program:eof" eof
  p2 <- getPos
  let ans = Node Program [l, m, o] p1 p2
  leave "program" ans
  return ans

pLicense :: Parser Node
pLicense = do
  p1 <- getPos
  enter "license"
  cs <- someTry (listNode $ do
    c <- debug "license:comment" pCOMMENT
    e <- pEOL
    return [c, e]
    )
  p2 <- getPos
  let ans = Node License cs p1 p2
  leave "license" ans
  return ans

pMetas :: Parser Node
pMetas = do
  p1 <- getPos
  enter "metas"
  ms <- someTry (listNode $ do
    c <- debug "metas:meta" pMETA
    e <- pEOL
    return [c, e])
  p2 <- getPos
  let ans = Node Metas ms p1 p2
  leave "metas" ans
  return ans

pObjects :: Parser Node
pObjects = do
  p1 <- getPos
  enter "objects"
  os <- someTry (listNode $ do
    c <- debug "objects:object" pObject
    e <- pEOL
    return [c, e])
  p2 <- getPos
  let ans = Node Objects os p1 p2
  leave "objects" ans
  return ans

enter :: Show a => a -> ParsecT Void Text Identity ()
enter name = do
  pos <- getPos
  debug (show pos <> ": Enter " <> show name) pEmpty
  return ()
leave :: (Show a1, Show a2) => a1 -> a2 -> ParsecT Void Text Identity ()
leave name node = do
  pos <- getPos
  let l = printf "%s: Leave %s" (show pos) (show name)
  debug l pEmpty
  return ()


pObject :: Parser Node
pObject = do
  p1 <- getPos
  enter "object"
  comments <- listNode $ manyTry (listNode $ do
    c <- debug "object:comment" pCOMMENT
    e <- pEOL
    return [c, e])
  a <-
    choiceTry
      [ debug "object:abstraction" pAbstraction,
        debug "object:application" pApplication
      ]
  t <- optionalNode (debug "object:tail" pTail)
  let g = do
        e <- pEOL
        method <- debug "object:method" pMethod
        h <- optionalNode (debug "object:htail" pHtail)
        suffix <- optionalNode (debug "object:suffix" pSuffix)
        p <- optionalNode (debug "object:tail" pTail)
        return [e, method, h, suffix, p]
  s <- listNode $ manyTry $ listNode (debug "object:after tail" g)
  p2 <- getPos
  let ans = Node Object [comments, a, t, s] p1 p2
  leave "object" ans
  return ans

pAbstraction :: Parser Node
pAbstraction = do
  p1 <- getPos
  enter "abstraction"
  attrs <- debug "abstraction:attributes" pAttributes
  t <-
    optionalNode $
      listNode $ choiceTry
          [ do
              suff <- debug "abstraction:suffix" pSuffix
              o <-
                optionalNode $
                  string cSPACE
                    *> string cSLASH
                    *> choiceTry
                      [ debug "abstraction:name" pNAME,
                        pTerminal cQUESTION QUESTION
                      ]
              return [suff, o],
            do
              htail <- debug "abstraction:htail" pHtail
              return [htail]
          ]
  p2 <- getPos
  let ans = Node Abstraction [attrs, t] p1 p2
  leave "abstraction" ans
  return ans

pAttributes :: Parser Node
pAttributes = do
  p1 <- getPos
  enter "attributes"
  _ <- string cLSQ
  let attrs = do
        a <- debug "attributes:attribute1" pAttribute
        as <- manyTry (string cSPACE *> debug "attributes:attribute2" pAttribute)
        return (a : as)
  attrs' <- optionalNode $ listNode attrs
  _ <- string cRSQ
  p2 <- getPos
  let ans = Node Attributes [attrs'] p1 p2
  leave "attributes" ans
  return ans

pAttribute :: Parser Node
pAttribute = pLabel

pLabel :: Parser Node
pLabel = do
  p1 <- getPos
  enter "label"
  l <-
    choiceTry
      [ (: []) <$> debug "label:@" (pTerminal cAT AT),
        do
          name <- debug "label:name" pNAME
          dots <- optionalNode (debug "label:..." (pTerminal cDOTS DOTS))
          return [name, dots]
      ]
  p2 <- getPos
  let ans = Node Label l p1 p2
  leave "label" ans
  return ans

pTail :: Parser Node
pTail = do
  p1 <- getPos
  enter "tail"
  e <- debug "tail:eol" pEOL
  objects <- listNode $ someTry ( listNode $ do
    o <- debug "tail:object" pObject
    e1 <- pEOL
    return [o, e1])
  p2 <- getPos
  let ans = Node Tail [e, objects] p1 p2
  leave "tail" ans
  return ans

pSuffix :: Parser Node
pSuffix = do
  p1 <- getPos
  enter "suffix"
  label <- string cSPACE *> string cARROW *> string cSPACE *> debug "suffix:label" pLabel
  c <- optionalNode (debug "suffix:const" (pTerminal cCONST CONST))
  p2 <- getPos
  let ans = Node Suffix [label, c] p1 p2
  leave "suffix" ans
  return ans

pMethod :: Parser Node
pMethod = do
  p1 <- getPos
  enter "method"
  method <-
    string cDOT
      *> choiceTry
        [ debug "method:name" pNAME,
          debug "method:^" (pTerminal cRHO RHO),
          debug "method:@" (pTerminal cAT AT),
          debug "method:<" (pTerminal cVERTEX VERTEX)
        ]
  p2 <- getPos
  let ans = Node Method [method] p1 p2
  leave "method" ans
  return ans

pApplication :: Parser Node
pApplication = do
  p1 <- getPos
  enter "application"
  s <-
    choiceTry
      [ debug "application:head" pHead,
        string cLB *> debug "application:application" pApplication <* string cRB
      ]
  h <- optionalNode (debug "application:htail" pHtail)
  a1 <- debug "application:application1" pApplication1
  p2 <- getPos
  let ans = Node Application [s, h, a1] p1 p2
  leave "application" ans
  return ans

pApplication1 :: Parser Node
pApplication1 = do
  p1 <- getPos
  enter "application1"
  c <-
    listNode $ choiceTry
        [ do
            c1 <-
              choiceTry
                [ debug "application1:method" pMethod,
                  debug "application1:has" pHas,
                  debug "application1:suffix" pSuffix
                ]
            ht <- optionalNode (debug "application1:htail" pHtail)
            a <- debug "application1:application1" pApplication1
            return [c1, ht, a],
          [] <$ pEmpty
        ]
  p2 <- getPos
  let ans = Node Application1 [c] p1 p2
  leave "application" ans
  return ans

pHtail :: Parser Node
pHtail = do
  p1 <- getPos
  enter "htail"
  let op =
        listNode $ choiceTry
            [ do
                h <- debug "htail:head" pHead
                return [h],
              do
                app <- debug "htail:application" pApplication
                m <-
                  choiceTry
                    [ debug "htail:method" pMethod,
                      debug "htail:has" pHas,
                      debug "htail:suffix" pSuffix
                    ]
                return [app, m],
              do
                app <- string cLB *> debug "htail:application1" pApplication <* string cRB
                return [app],
              do
                abstr <- debug "htail:abstraction" pAbstraction
                return [abstr]
            ]
  t <- someTry (string cSPACE *> op)
  p2 <- getPos
  let ans = Node Htail t p1 p2
  leave "htail" ans
  return ans

pHead :: Parser Node
pHead = do
  p1 <- getPos
  enter "head"
  dots <- optionalNode $ pTerminal cDOTS DOTS
  t <- listNode $
    choiceTry
      [ debug "head:root" ((:[]) <$> pTerminal cROOT ROOT),
        debug "head:at" ((:[]) <$> pTerminal cAT AT),
        debug "head:rho" ((:[]) <$> pTerminal cRHO RHO),
        debug "head:xi"  ((:[]) <$> pTerminal cXI XI),
        debug "head:sigma"  ((:[]) <$> pTerminal cSIGMA SIGMA),
        debug "head:star"  ((:[]) <$> pTerminal cSTAR STAR),
        debug "head:copy" (
          do
            name <- pNAME
            c <- choiceTry [
                  optionalNode $ pTerminal cCOPY COPY
                , pTerminal cDOT DOT
                ]
            return [name,c]),
        debug "head:data" ((:[]) <$> pDATA)
      ]
  p2 <- getPos
  let ans = Node Head [dots, t] p1 p2
  leave "head" ans
  return ans

pHas :: Parser Node
pHas = do
  p1 <- getPos
  enter "has"
  _ <- string cCOLON
  n <- debug "has:name" pNAME
  p2 <- getPos
  let ans = Node Has [n] p1 p2
  leave "has" ans
  return ans


pDATA :: Parser Node
pDATA = do
  p1 <- getPos
  enter "data"
  d <-
    choiceTry
      [ debug "data:bytes" pBYTES,
        debug "data:bool" pBOOL,
        debug "data:text" pTEXT,
        debug "data:string" pSTRING,
        debug "data:int" pINT,
        debug "data:float" pFLOAT,
        debug "data:hex" pHEX,
        debug "data:char" pCHAR,
        debug "data:regex" pREGEX
      ]
  p2 <- getPos
  let ans = Node Data [d] p1 p2
  leave "data" ans
  return ans

pCOMMENT :: Parser Node
pCOMMENT = do
  p1 <- getPos
  enter "comment"
  _ <- string cHASH
  content <- pack <$> many printChar
  p2 <- getPos
  let ans = Node (COMMENT content) [] p1 p2
  leave "comment" ans
  return ans

pMETA :: Parser Node
pMETA = do
  p1 <- getPos
  enter "comment"
  _ <- string cPLUS
  name <- debug "meta:name" pNAME
  suffix <- debug "meta:suffix" (optionalNode $ textNode $ pack <$> (string cSPACE *> some printChar))
  p2 <- getPos
  let ans = Node META [name, suffix] p1 p2
  leave "comment" ans
  return ans

pREGEX :: Parser Node
pREGEX = do
  p1 <- getPos
  enter "regex"
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  p2 <- getPos
  let ans = Node (REGEX r suffix) [] p1 p2
  leave "regex" ans
  return ans

pEOL :: Parser Node
pEOL = do
  p1 <- getPos
  enter "eol"
  _ <- debug "eol:eol" (eol *> optional eol)
  indents <- T.concat <$> many (string cINDENT)
  p2 <- getPos
  let nIndents = T.length indents `div` 2
  let ans = Node (INDENT nIndents) [] p1 p2
  leave "eol" ans
  return ans

pBYTE :: Parser Node
pBYTE = do
  p1 <- getPos
  enter "byte"
  b <- hexToInt <$> count 2 pHexDigitUpper
  p2 <- getPos
  let ans = Node (BYTE b) [] p1 p2
  leave "byte" ans
  return ans

pLINE_BYTES :: Parser Node
pLINE_BYTES = do
  p1 <- getPos
  enter "line bytes"
  byte <- debug "line_bytes:byte" pBYTE
  bytes <- debug "line_bytes:bytes" (someTry (string cMINUS *> pBYTE))
  p2 <- getPos
  let ans = Node LINE_BYTES (byte : bytes) p1 p2
  leave "line bytes" ans
  return ans

pBYTES :: Parser Node
pBYTES = do
  p1 <- getPos
  enter "bytes"
  bytes <-
    choiceTry
      [ parser1,
        parser3,
        parser2
      ]
  p2 <- getPos
  let ans = Node BYTES bytes p1 p2
  leave "bytes" ans
  return ans
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
      e <- pEOL
      lb <- pLINE_BYTES
      return [e, lb]
    parser3 = do
      lb <- pLINE_BYTES
      lbs <- concat <$> manyTry parser4
      return (lb : lbs)

pBOOL :: Parser Node
pBOOL = do
  p1 <- getPos
  enter "bool"
  b <-
    choiceTry
      [ BOOL True <$ string cTRUE,
        BOOL False <$ string cFALSE
      ]
  p2 <- getPos
  let ans = Node b [] p1 p2
  leave "bool" ans
  return ans

-- | slightly differs from grammar: doesn't allow u Byte Byte
pCHAR :: Parser Node
pCHAR = do
  p1 <- getPos
  -- enter "char"
  c <- char '\'' *> charLiteral <* char '\''
  p2 <- getPos
  let ans = Node (CHAR c) [] p1 p2
  -- leave "char" ans
  return ans

-- | slightly differs from grammar: doesn't allow u Byte Byte
pSTRING :: Parser Node
pSTRING = do
  p1 <- getPos
  enter "string"
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  p2 <- getPos
  let ans = Node (STRING s) [] p1 p2
  leave "string" ans
  return ans

pINT :: Parser Node
pINT = do
  p1 <- getPos
  enter "int"
  s <- signed pEmpty decimal
  p2 <- getPos
  let ans = Node (INT s) [] p1 p2
  leave "int" ans
  return ans

pFLOAT :: Parser Node
pFLOAT = do
  p1 <- getPos
  enter "float"
  f <- signed pEmpty scientific
  p2 <- getPos
  let ans = Node (FLOAT f) [] p1 p2
  leave "float" ans
  return ans

pHEX :: Parser Node
pHEX = do
  p1 <- getPos
  enter "hex"
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  p2 <- getPos
  let ans = Node (HEX s) [] p1 p2
  leave "hex" ans
  return ans

pNAME :: Parser Node
pNAME = do
  p1 <- getPos
  enter "name"
  l1 <- debug "name: first letter" lowerChar
  l2 <- debug "name: other letters" (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  p2 <- getPos
  let ans = Node (NAME (pack (l1 : l2))) [] p1 p2
  leave "name" ans
  return ans

-- IDK maybe need to allow indentation after eol
pTEXT :: Parser Node
pTEXT = do
  p1 <- getPos
  enter "text"
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  p2 <- getPos
  let ans = Node (TEXT t) [] p1 p2
  leave "text" ans
  return ans


main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  -- parseTest manyABandAc "ababac"
  parseTest (debug "program" pProgram) code
