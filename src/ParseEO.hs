{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module ParseEO (pProgram, Node (..), Position(..), TokenType (..)) where

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
                                             count, getSourcePos, many,
                                             manyTill, some, try, unPos, (<?>))
import           Text.Megaparsec.Char       (alphaNumChar, char, eol,
                                             hexDigitChar, letterChar,
                                             lowerChar, numberChar, printChar,
                                             string)
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
      ListNode -> printf "%s\n" (show nodeToken)
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

enter :: Show a => a -> ParsecT Void Text Identity ()
enter name = do
  pos <- getPos
  debug (show pos <> ": Enter " <> show name) pEmpty
  return ()

pEOL :: Parser ()
pEOL = try $ do 
  _ <- eol *> optional (try eol)
  return ()

leave :: (Show a1, Show a2) => a1 -> a2 -> ParsecT Void Text Identity ()
leave name node = do
  pos <- getPos
  let l = printf "%s: Leave %s" (show pos) (show name)
  debug l pEmpty
  return ()

noIndent :: Int
noIndent = 0

indentAdd :: Int
indentAdd = 1

-- | decorator with common for all nodes actions
dec :: Show a1 => a1 -> Parser (TokenType, [Node]) -> Parser Node
dec name p = do
  p1 <- getPos
  -- enter name
  (t,l) <- p
  p2 <- getPos
  let ans = Node t l p1 p2
  -- leave name ans
  return ans

getIndent :: Node -> Int
getIndent n =
  case n of
    Node {nodeToken = INDENT ind} -> ind
    _                             -> 0

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************

pProgram :: Parser Node
pProgram = dec "Program" $ do
  l <- optionalNode ({-debug "program:license"-} pLicense)
  m <- optionalNode ({-debug "program:metas"-} pMetas)
  o <- {-debug "program:objects"-} pObjects
  return (Program, [l, m, o])

pLicense :: Parser Node
pLicense = dec "License" $ do
  cs <- someTry (pCOMMENT <* pEOL)
  return (License, cs)

pMetas :: Parser Node
pMetas = dec "Metas" $ do
  ms <- someTry (pMETA <* pEOL)
  return (Metas, ms)

pObjects :: Parser Node
pObjects = dec "Objects" $ do
  -- os <- someTry $ pObject noIndent <* pEOL_TAB_MANY
  os <- someTry $ pObject noIndent <* pEOL
  return (Objects, os)

pObject :: Int -> Parser Node
pObject ind = dec "Object" $ do
  comments <- listNode $ manyTry $ do
    c <- {-debug "object:comment"-} pCOMMENT
    e <- pEOL_TAB_MANY
    guard $ getIndent e == ind
    return c
  a <-
    choiceTry
      [ {-debug "object:abstraction"-} pAbstraction,
        {-debug "object:application"-} pApplication
      ]
  let newIndent = ind + indentAdd
  t <- optionalNode ({-debug "object:tail"-} pTail newIndent)
  
  
  -- TODO specify indentation and guard
  let g = try $ do
        e <- pEOL_TAB_MANY
        guard $ getIndent e == ind
        method <- {-debug "object:method"-} pMethod
        h <- optionalNode ({-debug "object:htail"-} pHtail)
        suffix <- optionalNode ({-debug "object:suffix"-} pSuffix)
        p <- optionalNode ({-debug "object:tail"-} pTail (ind + indentAdd))
        return [e, method, h, suffix, p]
  s <- listNode $ manyTry $ listNode ({-debug "object:after tail"-} g)
  return (Object, [comments, a, t, s])

pAbstraction :: Parser Node
pAbstraction = dec "Abstraction" $ do
  attrs <- {-debug "abstraction:attributes"-} pAttributes
  t <-
    optionalNode $
      listNode $ choiceTry
          [ do
              suff <- {-debug "abstraction:suffix"-} pSuffix
              o <-
                optionalNode $
                  string cSPACE
                    *> string cSLASH
                    *> choiceTry
                      [ {-debug "abstraction:name"-} pNAME,
                        pTerminal cQUESTION QUESTION
                      ]
              return [suff, o],
            do
              htail <- {-debug "abstraction:htail"-} pHtail
              return [htail]
          ]
  return (Abstraction, [attrs, t])

pAttributes :: Parser Node
pAttributes = dec "Attributes" $ do
  _ <- string cLSQ
  let attrs = do
        a <- {-debug "attributes:attribute1"-} pAttribute
        as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} pAttribute)
        return (a : as)
  attrs' <- optionalNode $ listNode attrs
  _ <- string cRSQ
  return (Attributes, [attrs'])

pAttribute :: Parser Node
pAttribute = pLabel

pLabel :: Parser Node
pLabel = dec "Label" $ do
  l <-
    choiceTry
      [ (: []) <$> {-debug "label:@"-} (pTerminal cAT AT),
        do
          name <- {-debug "label:name"-} pNAME
          -- TODO move dots to abstraction end (before csq)
          dots <- optionalNode ({-debug "label:..."-} (pTerminal cDOTS DOTS))
          return [name, dots]
      ]
  return (Label, l)

pTail :: Int -> Parser Node
pTail ind = dec "Tail" $ do
  let pObj = do
        e <- {-debug "tail:eol"-} pEOL_TAB_MANY
        let ind1 = getIndent e
        guard $ ind1 == ind
        pObject ind1
  objects <- someTry pObj
  return (Tail, objects)

pSuffix :: Parser Node
pSuffix = dec "Suffix" $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} pLabel
  c <- optionalNode ({-debug "suffix:const"-} (pTerminal cCONST CONST))
  return (Suffix, [label, c])

pMethod :: Parser Node
pMethod = dec "Method" $ do
  method <-
    string cDOT
      *> choiceTry
        [ {-debug "method:name"-} pNAME,
          {-debug "method:^"-} (pTerminal cRHO RHO),
          {-debug "method:@"-} (pTerminal cAT AT),
          {-debug "method:<"-} (pTerminal cVERTEX VERTEX)
        ]
  return (Method, [method])

pApplication :: Parser Node
pApplication = dec "Application" $ do
  s <-
    choiceTry
      [ {-debug "application:head"-} pHead,
        string cLB *> {-debug "application:application"-} pApplication <* string cRB
      ]
  h <- optionalNode ({-debug "application:htail"-} pHtail)
  a1 <- {-debug "application:application1"-} pApplication1
  return (Application, [s, h, a1])

pApplication1 :: Parser Node
pApplication1 = dec "Application1" $ do
  c <-
    listNode $ choiceTry
        [ do
            c1 <-
              choiceTry
                [ {-debug "application1:method"-} pMethod,
                  {-debug "application1:has"-} pHas,
                  {-debug "application1:suffix"-} pSuffix
                ]
            ht <- optionalNode ({-debug "application1:htail"-} pHtail)
            a <- {-debug "application1:application1"-} pApplication1
            return [c1, ht, a]
          , [] <$ pEmpty
        ]
  return (Application1, [c])

pHtail :: Parser Node
pHtail = dec "Htail" $ do
  let op =
        listNode $ choiceTry
            [ do
                h <- {-debug "htail:head"-} pHead
                return [h],
              do
                app <- {-debug "htail:application"-} pApplication
                m <-
                  choiceTry
                    [ {-debug "htail:method"-} pMethod,
                      {-debug "htail:has"-} pHas,
                      {-debug "htail:suffix"-} pSuffix
                    ]
                return [app, m],
              do
                app <- string cLB *> {-debug "htail:application1"-} pApplication <* string cRB
                return [app],
              do
                abstr <- {-debug "htail:abstraction"-} pAbstraction
                return [abstr]
            ]
  t <- someTry (string cSPACE *> op)
  return (Htail, t)

pHead :: Parser Node
pHead = dec "Head" $ do
  dots <- optionalNode $ pTerminal cDOTS DOTS
  t <- listNode $
    choiceTry
      [ {-debug "head:root"-} ((:[]) <$> pTerminal cROOT ROOT),
        {-debug "head:at"-} ((:[]) <$> pTerminal cAT AT),
        {-debug "head:rho"-} ((:[]) <$> pTerminal cRHO RHO),
        {-debug "head:xi"-}  ((:[]) <$> pTerminal cXI XI),
        {-debug "head:sigma"-}  ((:[]) <$> pTerminal cSIGMA SIGMA),
        {-debug "head:star"-}  ((:[]) <$> pTerminal cSTAR STAR),
        {-debug "head:copy"-} (
          do
            name <- pNAME
            c <- choiceTry [
                  optionalNode $ pTerminal cCOPY COPY
                , pTerminal cDOT DOT
                ]
            return [name,c]),
        {-debug "head:data"-} ((:[]) <$> pDATA)
      ]
  return (Head, [dots, t])

pHas :: Parser Node
pHas = dec "Has" $ do
  _ <- string cCOLON
  n <- {-debug "has:name"-} pNAME
  return (Has, [n])


pDATA :: Parser Node
pDATA = dec "DATA" $ do
  d <-
    choiceTry
      [ {-debug "data:bool"-} pBOOL,
        {-debug "data:text"-} pTEXT,
        {-debug "data:hex"-} pHEX,
        {-debug "data:string"-} pSTRING,
        {-debug "data:float"-} pFLOAT,
        {-debug "data:int"-} pINT,
        {-debug "data:bytes"-} pBYTES,
        {-debug "data:char"-} pCHAR,
        {-debug "data:regex"-} pREGEX
      ]
  return (Data, [d])

pCOMMENT :: Parser Node
pCOMMENT = dec "COMMENT" $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return (COMMENT content, [])

pMETA :: Parser Node
pMETA = dec "META" $ do
  _ <- string cPLUS
  name <- {-debug "meta:name"-} pNAME
  suffix <- {-debug "meta:suffix"-} (optionalNode $ textNode $ pack <$> (string cSPACE *> some printChar))
  return (META, [name, suffix])

pREGEX :: Parser Node
pREGEX = dec "REGEX" $ do
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  return (REGEX r suffix, [])

pEOL_TAB_MANY :: Parser Node
pEOL_TAB_MANY = dec "EOL_TAB_MANY" $ do
  _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return (INDENT nIndents, [])

pBYTE :: Parser Node
pBYTE = dec "BYTE" $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return (BYTE b, [])

pLINE_BYTES :: Parser Node
pLINE_BYTES = dec "LINE_BYTES" $ do
  byte <- {-debug "line_bytes:byte"-} pBYTE
  bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> pBYTE))
  return (LINE_BYTES, byte : bytes)

pBYTES :: Parser Node
pBYTES = dec "BYTES" $ do
  bytes <-
    choiceTry
      [ parser1,
        parser3,
        parser2
      ]
  return (BYTES, bytes)
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
      e <- pEOL_TAB_MANY
      lb <- pLINE_BYTES
      return [e, lb]
    parser3 = do
      lb <- pLINE_BYTES
      lbs <- concat <$> manyTry parser4
      return (lb : lbs)

pBOOL :: Parser Node
pBOOL = dec "BOOL" $ do
  b <-
    choiceTry
      [ BOOL True <$ string cTRUE,
        BOOL False <$ string cFALSE
      ]
  return (b, [])

-- | slightly differs from grammar: doesn't allow u Byte Byte
pCHAR :: Parser Node
pCHAR = dec "CHAR" $ do
  c <- char '\'' *> charLiteral <* char '\''
  return (CHAR c, [])

-- | slightly differs from grammar: doesn't allow u Byte Byte
pSTRING :: Parser Node
pSTRING = dec "STRING" $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return (STRING s, [])

pINT :: Parser Node
pINT = dec "INT" $ do
  s <- signed pEmpty decimal
  return (INT s, [])

pFLOAT :: Parser Node
pFLOAT = dec "FLOAT" $ do
  f <- signed pEmpty scientific
  return (FLOAT f, [])

pHEX :: Parser Node
pHEX = dec "HEX" $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return (HEX s, [])

pNAME :: Parser Node
pNAME = dec "NAME" $ do
  l1 <- {-debug "name: first letter"-} lowerChar
  l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return (NAME (pack (l1 : l2)), [])

-- IDK maybe need to allow indentation after eol
pTEXT :: Parser Node
pTEXT = dec "TEXT" $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return (TEXT t, [])
