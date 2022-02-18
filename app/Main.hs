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
                                             count, empty, eof, getSourcePos,
                                             many, manyTill, parseTest, some,
                                             try, unPos, (<?>))
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
import Debug.Trace as DT ( trace )


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
  | SeveralNode
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
  void $ string s
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
  return
    n
      { start = p1,
        end = p2
      }

textNode :: Parser Text -> Parser Node
textNode txt = do
  p1 <- getPos
  t <- txt
  p2 <- getPos
  return
    initNode
      { nodeToken = TextNode t,
        start = p1,
        end = p2
      }

debugFlag :: Bool
debugFlag = True

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
  l <- optionalNode (debug "program:license" pLicense)
  m <- optionalNode (debug "program:metas" pMetas)
  o <- debug "program:objects" pObjects
  _ <- eof
  p2 <- getPos
  return $ Node Program [l, m, o] p1 p2

pLicense :: Parser Node
pLicense = do
  p1 <- getPos
  cs <- someTry (debug "license:comment" pCOMMENT <* pEOL)
  p2 <- getPos
  return $ Node License cs p1 p2

pMetas :: Parser Node
pMetas = do
  p1 <- getPos
  ms <- someTry (debug "metas:meta" pMETA <* pEOL)
  p2 <- getPos
  return $ Node Metas ms p1 p2

pObjects :: Parser Node
pObjects = do
  p1 <- getPos
  os <- someTry (debug "objects:object" pObject <* pEOL)
  p2 <- getPos
  return $ Node Objects os p1 p2

pObject :: Parser Node
pObject = do
  p1 <- getPos
  comments <- listNode <$> manyTry (debug "object:comment" pCOMMENT <* pEOL)
  a <-
    choiceTry
      [ debug "object:abstraction" pAbstraction,
        debug "object:application" pApplication
      ]
  t <- optionalNode (debug "object:tail" pTail)
  let g = do
        _ <- pEOL
        method <- debug "object:method" pMethod
        h <- optionalNode (debug "object:htail" pHtail)
        suffix <- optionalNode (debug "object:suffix" pSuffix)
        p <- optionalNode (debug "object:tail" pTail)
        return (listNode [method, h, suffix, p])
  s <- listNode <$> manyTry (debug "object:after tail" g)
  p2 <- getPos
  return $ Node Object [comments, a, t, s] p1 p2

pAbstraction :: Parser Node
pAbstraction = do
  p1 <- getPos
  attrs <- debug "abstraction:attributes" pAttributes
  t <-
    optionalNode $
      listNode
        <$> choiceTry
          [ do
              suff <- debug "abstraction:suffix" pSuffix
              o <- optionalNode (string cSPACE *> string cSLASH *> (debug "abstraction:name" pNAME <|> pTerminal cQUESTION QUESTION))
              return [suff, o],
            do
              htail <- debug "abstraction:htail" pHtail
              return [htail]
          ]
  p2 <- getPos
  return $ Node Abstraction [attrs, t] p1 p2

pAttributes :: Parser Node
pAttributes = do
  p1 <- getPos
  _ <- string cLSQ
  let attrs = do
        a <- debug "attributes:attribute1" pAttribute
        as <- manyTry (string cSPACE *> debug "attributes:attribute2" pAttribute)
        return (listNode (a : as))
  attrs' <- optionalNode attrs
  _ <- string cRSQ
  p2 <- getPos
  return $ Node Attributes [attrs'] p1 p2

pAttribute :: Parser Node
pAttribute = pLabel

pLabel :: Parser Node
pLabel = do
  p1 <- getPos
  l <-
    choiceTry
      [ (: []) <$> debug "label:@" (pTerminal cAT AT),
        do
          name <- debug "label:name" pNAME
          dots <- optionalNode (debug "label:..." (pTerminal cDOTS DOTS))
          return [name, dots]
      ]
  p2 <- getPos
  return $ Node Label l p1 p2

pTail :: Parser Node
pTail = do
  p1 <- getPos
  e <- debug "tail:eol_indent" pEOL
  objects <- listNode <$> someTry (debug "tail:object" pObject <* pEOL)
  p2 <- getPos
  return $ Node Tail [e, objects] p1 p2

pSuffix :: Parser Node
pSuffix = do
  p1 <- getPos
  label <- string cSPACE *> string cARROW *> string cSPACE *> debug "suffix:label" pLabel
  c <- optionalNode (debug "suffix:const" (pTerminal cCONST CONST))
  p2 <- getPos
  return $ Node Suffix [label, c] p1 p2

pMethod :: Parser Node
pMethod = do
  p1 <- getPos
  method <-
    string cDOT
      *> choiceTry
        [ debug "method:name" pNAME,
          debug "method:^" (pTerminal cRHO RHO),
          debug "method:@" (pTerminal cAT AT),
          debug "method:<" (pTerminal cVERTEX VERTEX)
        ]
  p2 <- getPos
  return $ Node Method [method] p1 p2

pApplication :: Parser Node
pApplication = do
  p1 <- getPos
  s <-
    choiceTry
      [ debug "application:head" pHead,
        string cLB *> debug "application:application" pApplication <* string cRB
      ]
  h <- optionalNode (debug "application:htail" pHtail)
  a1 <- debug "application:application1" pApplication1
  p2 <- getPos
  return $ Node Application [s, h, a1] p1 p2

pApplication1 :: Parser Node
pApplication1 = do
  p1 <- getPos
  c <-
    listNode
      <$> choiceTry
        [ do
            c1 <-
              choiceTry
                [ debug "application1:method" pMethod,
                  debug "application1:has" pHas,
                  debug "application1:suffix" pSuffix
                ]
            ht <- optionalNode (debug "application1:htail" pHtail)
            a <- debug "application1:application" pApplication1
            return [c1, ht, a],
          [] <$ pEmpty
        ]
  p2 <- getPos
  return $ Node Application1 [c] p1 p2

pHtail :: Parser Node
pHtail = do
  p1 <- getPos
  let op =
        listNode
          <$> choiceTry
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
  return $ Node Htail t p1 p2

pHead :: Parser Node
pHead = do
  p1 <- getPos
  dots <- optionalNode $ pTerminal cDOTS DOTS
  t <-
    choiceTry
      [ debug "head:root" $ pTerminal cROOT ROOT,
        debug "head:at" $ pTerminal cAT AT,
        debug "head:rho" $ pTerminal cRHO RHO,
        debug "head:xi" $ pTerminal cXI XI,
        debug "head:sigma" $ pTerminal cSIGMA SIGMA,
        debug "head:star" $ pTerminal cSTAR STAR,
        debug "head:copy" $ pNAME <* optional (string cCOPY),
        debug "head:name" $ pNAME <* string cDOT,
        debug "head:data" $ pDATA
      ]
  p2 <- getPos
  return $ Node Head [dots, t] p1 p2

pHas :: Parser Node
pHas = do
  p1 <- getPos
  _ <- string cCOLON
  n <- debug "has:name" pNAME
  p2 <- getPos
  return $ Node Has [n] p1 p2

pDATA :: Parser Node
pDATA = do
  p1 <- getPos
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
  return $ Node Data [d] p1 p2

pCOMMENT :: Parser Node
pCOMMENT = do
  p1 <- getPos
  _ <- string cHASH
  content <- pack <$> many printChar
  p2 <- getPos
  return $ Node (COMMENT content) [] p1 p2

pMETA :: Parser Node
pMETA = do
  p1 <- getPos
  _ <- string cPLUS
  name <- debug "meta:name" pNAME
  suffix <- debug "meta:suffix" $ optionalNode . textNode $ pack <$> (string cSPACE *> many printChar)
  p2 <- getPos
  return $ Node META [name, suffix] p1 p2

pREGEX :: Parser Node
pREGEX = do
  p1 <- getPos
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  p2 <- getPos
  return $ Node (REGEX r suffix) [] p1 p2

pEOL :: Parser Node
pEOL = do
  p1 <- getPos
  _ <- debug "eol_indent:eol" (some eol)
  indents <- T.concat <$> many (string cINDENT)
  p2 <- getPos
  let nIndents = T.length indents `div` 2
  return $ Node (INDENT nIndents) [] p1 p2

pBYTE :: Parser Node
pBYTE = do
  p1 <- getPos
  b <- hexToInt <$> count 2 pHexDigitUpper
  p2 <- getPos
  return $ Node (BYTE b) [] p1 p2

pLINE_BYTES :: Parser Node
pLINE_BYTES = do
  p1 <- getPos
  byte <- debug "line_bytes:byte" pBYTE
  bytes <- debug "line_bytes:bytes" $ someTry (string cMINUS *> pBYTE)
  p2 <- getPos
  return $ Node LINE_BYTES (byte : bytes) p1 p2

pBYTES :: Parser Node
pBYTES = do
  p1 <- getPos
  bytes <-
    choiceTry
      [ parser1,
        parser3,
        parser2
      ]
  p2 <- getPos
  return $ Node BYTES bytes p1 p2
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
  b <-
    choiceTry
      [ BOOL True <$ string cTRUE,
        BOOL False <$ string cFALSE
      ]
  p2 <- getPos
  return $ Node b [] p1 p2

-- | slightly differs from grammar: doesn't allow u Byte Byte
pCHAR :: Parser Node
pCHAR = do
  p1 <- getPos
  c <- char '\'' *> charLiteral <* char '\''
  p2 <- getPos
  return $ Node (CHAR c) [] p1 p2

-- | slightly differs from grammar: doesn't allow u Byte Byte
pSTRING :: Parser Node
pSTRING = do
  p1 <- getPos
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  p2 <- getPos
  return $ Node (STRING s) [] p1 p2

pINT :: Parser Node
pINT = do
  p1 <- getPos
  s <- signed pEmpty decimal
  p2 <- getPos
  return $ Node (INT s) [] p1 p2

pFLOAT :: Parser Node
pFLOAT = do
  p1 <- getPos
  f <- signed pEmpty scientific
  p2 <- getPos
  return $ Node (FLOAT f) [] p1 p2

pHEX :: Parser Node
pHEX = do
  p1 <- getPos
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  p2 <- getPos
  return $ Node (HEX s) [] p1 p2

pNAME :: Parser Node
pNAME = do
  p1 <- getPos
  l1 <- debug "name: first letter" lowerChar
  l2 <- debug "name: other letters" $ many (letterChar <|> numberChar <|> char '_' <|> char '-')
  p2 <- getPos
  return $ Node (NAME (pack (l1 : l2))) [] p1 p2

-- IDK maybe need to allow indentation after eol
pTEXT :: Parser Node
pTEXT = do
  p1 <- getPos
  t <- try $ pack <$> (string "\"\"\"" *> eol *> manyTill charLiteral (string "\"\"\""))
  p2 <- getPos
  return $ Node (TEXT t) [] p1 p2

-- manyABandAc :: Parser [String]
-- manyABandAc = do
--   -- let p = char 'a' *> char 'b'
--   m <- many ( try $ do
--     a <- char 'a'
--     b <- char 'b'
--     return ([a,b]::String))
--   s <- do
--     a <- char 'a'
--     c <- char 'c'
--     return ([a,c]::String)
--   return (s:m)

main :: IO ()
main = do
  let file = "./app/code.eo"
  code <- pack <$> readFile file
  putStrLn "\n"
  -- parseTest manyABandAc "ababac"
  parseTest (debug "program" pProgram) code
