{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ParseEO
  ( tProgram,
    Load (..),
    I,
    Node (..),
    Position (..),
    TProgram (..),
    TLicense (..),
    TComment (..),
    TMetas (..),
    TMeta (..),
    TMetaSuffix (..),
    TName (..),
    TObjects (..),
    TObject (..),
    TAbstraction (..),
    TTail (..),
    TApplication (..),
    TApplication1 (..),
    TApplication1Elem (..),
    TMethod (..),
    THas (..),
    TAttributes (..),
    TAbstractionTail (..),
    THtail (..),
    TLabel (..),
    TSuffix (..),
    THead (..),
    THeadName (..),
    TData (..),
    TBool (..),
    TText (..),
    THex (..),
    TString (..),
    TFloat (..),
    TInt (..),
    TBytes (..),
    TChar (..),
    TRegex (..),
    TLineBytes (..),
    TByte (..),
    TIndent (..),
    Options2 (..),
    Options3 (..),
    Options9 (..),
    TDots (..),
    TConst (..),
    TFreeAttribute (..),
    TVarArg (..),
    THeadTerminal(..),
    TMethodTerminal(..),
    TLabelTerminal(..),
    THeadModifier(..),
    TAbstrQuestion(..),
    TRegexBody(..),
    TRegexSuffix(..),
    TObjectTail(..),
    cAT,
    cDOTS,
    cRHO,
    cVERTEX,
    cROOT,
    cSIGMA,
    cSTAR,
    cXI,
    cCOPY,
    cDOT,
    cCONST,
    cQUESTION
  )
where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad.Identity (Identity, guard, void)
import Data.Char (digitToInt)
import Data.Data (Data (toConstr))
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (lookAhead, takeWhile1P, notFollowedBy),
    Parsec,
    SourcePos (SourcePos),
    choice,
    count,
    getSourcePos,
    many,
    manyTill,
    some,
    try,
    unPos,
    (<?>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    eol,
    hexDigitChar,
    letterChar,
    lowerChar,
    numberChar,
    printChar,
    string,
  )
import Text.Megaparsec.Char.Lexer
  ( charLiteral,
    decimal,
    scientific,
    signed,
  )
-- import Text.Megaparsec.Debug (dbg)
-- import qualified Text.Megaparsec.Error
-- import Text.Megaparsec.Internal (ParsecT)
-- import qualified Text.Megaparsec.Stream as TM
import Text.Printf (printf)

type Parser = Parsec Void Text

-- constant symbols

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

-- Node definition

data Position = Position
  { row :: Int,
    column :: Int
  }
  deriving (Data)

data Segment = Segment {start :: Position, end :: Position} deriving (Data)

type I a = Node a Load

data Load = Load {num :: Int} deriving (Data)

initLoad :: Load
initLoad = Load {num = 0}

data Node a b = Node {pos :: Segment, node :: a, load :: b} deriving (Data)

instance Show Position where
  show (Position r c) = printf "%d:%d" r c

instance Show Segment where
  show Segment {..} = printf "[%s..%s]" (show start) (show end)

instance Show Load where
  show Load {..} = "(" <> show num <> ")"

-- Parsing helpers

getPos :: Parser Position
getPos = do
  SourcePos _ r c <- getSourcePos
  return (Position (unPos r) (unPos c))

tHexDigit :: Char -> Char -> Parser Int
tHexDigit l1 l2 = do
  c <- hexDigitChar
  guard (c `elem` ['0' .. '9'] <> [l1 .. l2])
  let c' = digitToInt c
  return c'

pHexDigitUpper :: Parser Integer
pHexDigitUpper = toInteger <$> tHexDigit 'A' 'F'

pHexDigitLower :: Parser Integer
pHexDigitLower = toInteger <$> tHexDigit 'a' 'f'

pEmpty :: Parser ()
pEmpty = return ()

hexToInt :: [Integer] -> Integer
hexToInt = foldl (\x acc -> (acc * 16) + x) 0

debugFlag :: Bool
debugFlag = True

data DebugMode = On | Off

-- debug :: (Show a, Text.Megaparsec.Error.ShowErrorComponent e,
--  TM.VisualStream s) =>
--   String -> ParsecT e s m a -> ParsecT e s m a
-- debug :: (Show a, Text.Megaparsec.Error.ShowErrorComponent e,
--  TM.VisualStream s) =>
--   String -> ParsecT e s m a -> ParsecT e s m a
-- debug label parser
--   | debugFlag = dbg label parser
--   | otherwise = parser <?> label

manyTry :: MonadParsec e s m => m a -> m [a]
manyTry p = try $ many (try p)

someTry :: MonadParsec e s m => m a -> m [a]
someTry p = try $ some (try p)

choiceTry :: MonadParsec e s m => [m a] -> m a
choiceTry p = try $ choice (map try p)

-- | print a debug message when enter a node
-- enter :: Show a => a -> ParsecT Void Text Identity ()
-- enter name = do
--   pos <- getPos
--   debug (show pos <> ": Enter " <> show name) pEmpty
--   return ()

tEOL :: Parser ()
tEOL = try $ do
  _ <- eol *> optional (try eol)
  return ()

-- | print a debug message when leave a node
-- leave :: Show a => a -> p -> ParsecT Void Text Identity ()
-- leave name node = do
--   pos <- getPos
--   let l = printf "%s: Leave %s" (show pos) (show name)
--   debug l pEmpty
--   return ()

noIndent :: Int
noIndent = 0

indentAdd :: Int
indentAdd = 1

getIndent :: I TIndent -> Int
getIndent Node {node = TIndent {..}} = n

-- | does common actions for all nodes
dec :: Text -> Parser a -> Parser (I a)
dec t p = do
  p1 <- getPos
  -- enter t
  p' <- p
  p2 <- getPos
  let ans = Node (Segment p1 p2) p' initLoad
  -- leave t ans
  return ans

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************

data Options2 a b = Opt2A a | Opt2B b deriving (Data)

data Options3 a b c = Opt3A a | Opt3B b | Opt3C c deriving (Data)

instance (Show a, Show b, Show c) => Show (Options3 a b c) where
  show (Opt3A a) = show a
  show (Opt3B a) = show a
  show (Opt3C a) = show a

instance (Show a, Show b) => Show (Options2 a b) where
  show (Opt2A a) = show a
  show (Opt2B a) = show a

data TProgram = TProgram {l :: Maybe (I TLicense), m :: Maybe (I TMetas), o :: I TObjects} deriving (Data)

tProgram :: Parser (I TProgram)
tProgram = dec "Program" $ do
  l <- optional $ try ({-debug "program:license"-} tLicense)
  m <- optional $ try ({-debug "program:metas"-} tMetas)
  o <- {-debug "program:objects"-} tObjects
  return TProgram {l = l, m = m, o = o}

data TLicense = TLicense {cs :: [I TComment]} deriving (Data)

tLicense :: Parser (I TLicense)
tLicense = dec "License" $ do
  cs <- someTry (tComment <* tEOL)
  return TLicense {cs = cs}

data TMetas = TMetas {ms :: [I TMeta]} deriving (Data)

tMetas :: Parser (I TMetas)
tMetas = dec "Metas" $ do
  ms <- someTry (tMeta <* tEOL)
  return TMetas {ms = ms}

data TObjects = TObjects {os :: [I TObject]} deriving (Data)

tObjects :: Parser (I TObjects)
tObjects = dec "Objects" $ do
  os <- someTry $ tObject noIndent <* tEOL
  return TObjects {os = os}

data TObjectTail = TObjectTail {m :: I TMethod, h :: Maybe (I THtail), s :: Maybe (I TSuffix), t :: Maybe (I TTail)} deriving (Data)

data TObject = TObject
  { cs :: [I TComment],
    a :: Options2 (I TAbstraction) (I TApplication),
    t :: Maybe (I TTail),
    s :: [I TObjectTail]
  }
  deriving (Data)

tObject :: Int -> Parser (I TObject)
tObject ind = dec "Object" $ do
  comments <- manyTry $ do
    c <- {-debug "object:comment"-} tComment
    e <- tEOLTabMany
    guard $ getIndent e == ind
    return c
  a <-
    choiceTry
      [ Opt2A <$> {-debug "object:abstraction"-} tAbstraction,
        Opt2B <$> {-debug "object:application"-} tApplication
      ]
  let newIndent = ind + indentAdd
  -- list of attributes
  t <- optional $ try ({-debug "object:tail"-} tTail newIndent)
  let g1 = do
        e <- tEOLTabMany
        guard $ getIndent e == ind
        method <- {-debug "object:method"-} tMethod
        h <- optional $ try ({-debug "object:htail"-} tHtail)
        suffix <- optional $ try ({-debug "object:suffix"-} tSuffix)
        p <- optional $ try ({-debug "object:tail"-} tTail (ind + indentAdd))
        dec "ObjectTail" $ return TObjectTail { m = method, h = h, s = suffix, t = p}
  s <- manyTry $ ({-debug "object:after tail"-} g1)
  return TObject {cs = comments, a = a, t = t, s = s}

-- TODO use separate parser for free attributes
data TAbstraction = TAbstraction {as :: I TAttributes, t :: Maybe (I TAbstractionTail)} deriving (Data)

tAbstraction :: Parser (I TAbstraction)
tAbstraction = dec "Abstraction" $ do
  attrs <- tAttributes
  t <- optional $ try tAbstractionTail
  return TAbstraction {as = attrs, t = t}

-- TODO Use separate terminal for question


data TAbstractionTail = TAbstractionTail {e :: Options2 (I TSuffix, Maybe (Options2 (I TName) (I TAbstrQuestion))) (I THtail)} deriving (Data)
data TAbstrQuestion = 
  AbstrQuestion
  deriving (Data)

instance Show TAbstrQuestion where
  show t = constructorName t

instance Show TDots where
  show t = constructorName t

instance Show TConst where
  show t = constructorName t

instance Show TMethodTerminal where
  show t = drop 6 (constructorName t)

instance Show TLabelTerminal where
  show t = drop 5 (constructorName t)

instance Show THeadTerminal where
  show t = drop 4 (constructorName t)

instance Show THeadModifier where
  show t = drop 4 (constructorName t)

tAbstractionTail :: Parser (I TAbstractionTail)
tAbstractionTail = dec "Abstraction tail" $ do
  let a = do
        suff <- tSuffix
        o <-
          optional $
            try
              ( string cSPACE
                  *> string cSLASH
                  *> choiceTry
                    [ Opt2A <$> {-debug "abstraction:name"-} tName,
                      Opt2B <$> tTerminal cQUESTION AbstrQuestion
                    ]
              )
        return (suff, o)
  let b = tHtail
  e <- choiceTry [Opt2A <$> a, Opt2B <$> b]
  return TAbstractionTail {e = e}

data TAttributes = TAttributes {as :: [I TFreeAttribute]} deriving (Data)

-- | list of arguments
--
-- If no arguments are provided, the list is empty
--
-- This is the same as making the part between [] optional
tAttributes :: Parser (I TAttributes)
tAttributes = dec "Attributes" $ do
  _ <- string cLSQ
  attrs <-
    choiceTry
      [ do
          a <- {-debug "attributes:attribute1"-} tFreeAttribute
          as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} tFreeAttribute)

          -- last argument may be vararg
          d <- optional $ try (tTerminal cDOTS TDots)
          let as' = a : as
          let p@Node {node = TFreeAttribute {..}} = last as'
          let l' =
                case d of
                  Just _ ->
                    case l of
                      Opt3A _ -> error "decoratee cannot be vararg"
                      Opt3B n1@Node {node = TName {..}} -> Opt3C n1 {node = TVarArg n}
                      _ -> error "couldn't have parsed dots in free attributes"
                  _ -> l
          return (init as' ++ [p {node = TFreeAttribute l'}]),
        [] <$ pEmpty
      ]
  _ <- string cRSQ
  return TAttributes {as = attrs}

data TVarArg = TVarArg {n :: Text} deriving (Data)

data TFreeAttribute = TFreeAttribute {l :: Options3 (I TLabelTerminal) (I TName) (I TVarArg)} deriving (Data)

tFreeAttribute :: Parser (I TFreeAttribute)
tFreeAttribute = dec "Free Attribute" $ do
  l <-
    choiceTry
      [ Opt3A <$> {-debug "label:@"-} (tTerminal cAT LabelAt),
        Opt3B <$> {-debug "label:name"-} (tName)
      ]
  return TFreeAttribute {l = l}


data TLabel = TLabel {l :: Options2 (I TLabelTerminal) (I TName, Maybe (I TDots))} deriving (Data)

data TLabelTerminal = 
    LabelAt
  deriving (Data)

tLabel :: Parser (I TLabel)
tLabel = dec "Label" $ do
  l <-
    choiceTry
      [ Opt2A <$> {-debug "label:@"-} (tTerminal cAT LabelAt),
        Opt2B
          <$> ( do
                  name <- {-debug "label:name"-} tName
                  -- TODO move dots to abstraction end (before csq)
                  dots <- optional ({-debug "label:..."-} (tTerminal cDOTS TDots))
                  return (name, dots)
              )
      ]
  return TLabel {l = l}

data TTail = TTail {os :: [I TObject]} deriving (Data)

tTail :: Int -> Parser (I TTail)
tTail ind = dec "Tail" $ do
  let tObj = do
        e <- {-debug "tail:eol"-} tEOLTabMany
        let ind1 = getIndent e
        guard $ ind1 == ind
        tObject ind1
  objects <- someTry tObj
  return TTail {os = objects}

data TSuffix = TSuffix {l :: I TLabel, c :: Maybe (I TConst)} deriving (Data)

tSuffix :: Parser (I TSuffix)
tSuffix = dec "Suffix" $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} tLabel
  c <- optional ({-debug "suffix:const"-} (tTerminal cCONST TConst))
  return TSuffix {l = label, c = c}

data TMethodTerminal = 
    MethodRho
  | MethodAt
  | MethodVertex
  deriving (Data)

-- TODO separate set of terminals for Method
data TMethod = TMethod {m :: Options2 (I TName) (I TMethodTerminal)} deriving (Data)

tMethod :: Parser (I TMethod)
tMethod = dec "Method" $ do
  method <-
    string cDOT
      *> choiceTry
        [ Opt2A <$> {-debug "method:name"-} tName,
          Opt2B <$> {-debug "method:^"-} (tTerminal cRHO MethodRho),
          Opt2B <$> {-debug "method:@"-} (tTerminal cAT MethodAt),
          Opt2B <$> {-debug "method:<"-} (tTerminal cVERTEX MethodVertex)
        ]
  return TMethod {m = method}

data TApplication = TApplication {s :: Options2 (I THead) (I TApplication), h :: Maybe (I THtail), a1 :: I TApplication1} deriving (Data)

tApplication :: Parser (I TApplication)
tApplication = dec "Application" $ do
  s <-
    choiceTry
      [ Opt2A <$> {-debug "application:head"-} tHead,
        Opt2B <$> (string cLB *> {-debug "application:application"-} tApplication <* string cRB)
      ]
  h <- optional $ try ({-debug "application:htail"-} tHtail)
  a1 <- {-debug "application:application1"-} tApplication1
  return TApplication {s = s, h = h, a1 = a1}


data TApplication1 = TApplication1 {c :: Maybe (I TApplication1Elem)} deriving (Data)

-- | appeared after fixing left recursion
tApplication1 :: Parser (I TApplication1)
tApplication1 = dec "Application1" $ do
  -- this rule can read an empty string, that's why here stands 'optional'
  c <- optional $ try tApplication1Elem
  return TApplication1 {c = c}

data TApplication1Elem = TApplication1Elem {c1 :: Options3 (I TMethod) (I THas) (I TSuffix), ht :: Maybe (I THtail), a :: I TApplication1} deriving (Data)

tApplication1Elem :: Parser (I TApplication1Elem)
tApplication1Elem = dec "Application1 Element" $ do
  c1 <-
    choiceTry
      [ Opt3A <$> {-debug "application1:method"-} tMethod,
        Opt3B <$> {-debug "application1:has"-} tHas,
        Opt3C <$> {-debug "application1:suffix"-} tSuffix
      ]
  ht <- optional $ try ({-debug "application1:htail"-} tHtail)
  a <- {-debug "application1:application1"-} tApplication1
  return TApplication1Elem {c1 = c1, ht = ht, a = a}

data THtail = THtail {t :: [Options3 (I THead) (I TApplication) (I TAbstraction)]} deriving (Data)

{- | 
-- TODO

Currently, we don't parse application followed by method, has, or suffix.

This allows for left associative argument lists
-}
tHtail :: Parser (I THtail)
tHtail = dec "Htail" $ do
  let op =
        choiceTry
          [ Opt3A <$> {-debug "htail:head"-} tHead,
            Opt3B <$> (string cLB *> {-debug "htail:application1"-} tApplication <* string cRB),
            Opt3C <$> (string cLB *> {-debug "htail:abstraction"-} tAbstraction <* string cRB)
          ]
  t <- someTry (string cSPACE *> op)
  return THtail {t = t}

data TDots = TDots deriving (Data)

data TConst = TConst deriving (Data)

tTerminal :: Text -> a -> Parser (I a)
tTerminal s t = dec s $ do
  p1 <- getPos
  void (string s)
  p2 <- getPos
  return t

-- TODO prohibit dots before data
data THead = THead {dots :: Maybe (I TDots), t :: Options3 (I THeadTerminal) (I THeadName) (I TData)} deriving (Data)

data THeadTerminal = 
    HeadRoot
  | HeadAt
  | HeadRho
  | HeadXi
  | HeadSigma
  | HeadStar
  deriving (Data)

tHead :: Parser (I THead)
tHead = dec "Head" $ do
  dots <- optional $ tTerminal cDOTS TDots
  t <-
    choiceTry
      [ Opt3A <$> {-debug "head:root"-} (tTerminal cROOT HeadRoot),
        Opt3A <$> {-debug "head:at"-} (tTerminal cAT HeadAt),
        Opt3A <$> {-debug "head:rho"-} (tTerminal cRHO HeadRho),
        Opt3A <$> {-debug "head:xi"-} (tTerminal cXI HeadXi),
        Opt3A <$> {-debug "head:sigma"-} (tTerminal cSIGMA HeadSigma),
        Opt3A <$> {-debug "head:star"-} (tTerminal cSTAR HeadStar),
        Opt3B <$> tHeadName,
        Opt3C <$> {-debug "head:data"-} (tData)
      ]
  return THead {dots = dots, t = t}

-- TODO lookahead EOL
{- | head name with a

dot: a.

copy: a'

-}

data THeadName = THeadName {name :: I TName, c :: Maybe (I THeadModifier)} deriving (Data)

data THeadModifier = 
    HeadDot
  | HeadCopy
  deriving (Data)

tHeadName :: Parser (I THeadName)
tHeadName = dec "Head name" $ do
  name <- tName
  c <-
    choiceTry
      [ Just <$> tTerminal cDOT HeadDot <* lookAhead (string cSPACE),
        optional (tTerminal cCOPY HeadCopy)
      ]
  return THeadName {name = name, c = c}

data THas = THas {n :: I TName} deriving (Data)

tHas :: Parser (I THas)
tHas = dec "Has" $ do
  _ <- string cCOLON
  n <- {-debug "has:name"-} tName
  return THas {n = n}

data Options9 a b c d e f g h i
  = Opt9A a
  | Opt9B b
  | Opt9C c
  | Opt9D d
  | Opt9E e
  | Opt9F f
  | Opt9G g
  | Opt9H h
  | Opt9I i
  deriving (Data)

data TData = TData {d :: Options9 (I TBool) (I TText) (I THex) (I TString) (I TFloat) (I TInt) (I TBytes) (I TChar) (I TRegex)} deriving (Data)

tData :: Parser (I TData)
tData = dec "DATA" $ do
  d <-
    choiceTry
      [ {-debug "data:bool"-} Opt9A <$> tBool,
        {-debug "data:text"-} Opt9B <$> tText,
        {-debug "data:hex"-} Opt9C <$> tHex,
        {-debug "data:string"-} Opt9D <$> tString,
        {-debug "data:int"-} Opt9F <$> tInt <* notFollowedBy (string cDOT),
        {-debug "data:float"-} Opt9E <$> tFloat,
        {-debug "data:bytes"-} Opt9G <$> tBytes,
        {-debug "data:char"-} Opt9H <$> tChar,
        {-debug "data:regex"-} Opt9I <$> tRegex
      ]
  return TData {d = d}

data TComment = TComment {c :: Text} deriving (Data)

tComment :: Parser (I TComment)
tComment = dec "COMMENT" $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return TComment {c = content}

data TMeta = TMeta {name :: I TName, suff :: Maybe (I TMetaSuffix)} deriving (Data)

tMeta :: Parser (I TMeta)
tMeta = dec "META" $ do
  _ <- string cPLUS
  name <- {-debug "meta:name"-} tName
  suffix <- {-debug "meta:suffix"-} (optional $ try tMetaSuffix)
  return TMeta {name = name, suff = suffix}

data TMetaSuffix = TMetaSuffix {s :: Text} deriving (Data)

tMetaSuffix :: Parser (I TMetaSuffix)
tMetaSuffix = dec "Meta Suffix" $ do
  suffix <- {-debug "meta:suffix"-} (pack <$> (string cSPACE *> some printChar))
  return TMetaSuffix {s = suffix}

data TRegexBody = TRegexBody {b :: Text} deriving (Data)
data TRegexSuffix = TRegexSuffix {s:: Text} deriving (Data)

-- TODO Regex nodes
data TRegex = TRegex {r :: I TRegexBody, suff :: I TRegexSuffix} deriving (Data)

tRegex :: Parser (I TRegex)
tRegex = dec "REGEX" $ do
  _ <- string cSLASH
  r <- dec "REGEX:Body" $ TRegexBody <$> takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- dec "REGEX:Suffix" $ TRegexSuffix <$> pack <$> many alphaNumChar
  return (TRegex {r = r, suff = suffix})

data TIndent = TIndent {n :: Int} deriving (Data)

tEOLTabMany :: Parser (I TIndent)
tEOLTabMany = dec "EOL_TAB_MANY" $ do
  _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return TIndent {n = nIndents}

data TByte = TByte {b :: Integer} | EmptyBytes deriving (Data)

tByte :: Parser (I TByte)
tByte = dec "BYTE" $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return TByte {b = b}

data TLineBytes = TLineBytes {bs :: [I TByte]} deriving (Data)

tLineBytes :: Parser (I TLineBytes)
tLineBytes = dec "LINE_BYTES" $ do
  byte <- {-debug "line_bytes:byte"-} tByte
  bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> tByte))
  return TLineBytes {bs = byte : bytes}

data TBytes = TBytes {bs :: Options2 (I TByte) [I TLineBytes]} deriving (Data)

tBytes :: Parser (I TBytes)
tBytes = dec "BYTES" $ do
  bytes <-
    choiceTry
      [ parser1,
        parser3,
        parser2
      ]
  return TBytes {bs = bytes}
  where
    parser1 = do
      _ <- string cEMPTY_BYTES
      emp <- dec "EMPTY_BYTES" (EmptyBytes <$ string cEMPTY_BYTES)
      return (Opt2A emp)
    parser2 = do
      byte <- tByte
      _ <- string cMINUS
      return (Opt2A byte)
    parser4 = do
      _ <- string cMINUS
      -- TODO guard indentation
      e <- tEOLTabMany
      lb <- tLineBytes
      return lb
    parser3 = do
      lb <- tLineBytes
      lbs <- manyTry parser4
      return (Opt2B (lb : lbs))

data TBool = TBool {b :: Bool} deriving (Data)

tBool :: Parser (I TBool)
tBool = dec "BOOL" $ do
  b <-
    choiceTry
      [ True <$ string cTRUE,
        False <$ string cFALSE
      ]
  return TBool {b = b}

data TChar = TChar {c :: Char} deriving (Data)

-- | slightly differs from grammar: doesn't allow u Byte Byte
tChar :: Parser (I TChar)
tChar = dec "CHAR" $ do
  c <- char '\'' *> charLiteral <* char '\''
  return TChar {c = c}

data TString = TString {s :: Text} deriving (Data)

-- | slightly differs from grammar: doesn't allow u Byte Byte
tString :: Parser (I TString)
tString = dec "STRING" $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return TString {s = s}

data TInt = TInt {s :: Integer} deriving (Data)

tInt :: Parser (I TInt)
tInt = dec "INT" $ do
  s <- signed pEmpty decimal
  return TInt {s = s}

data TFloat = TFloat {f :: Scientific} deriving (Data)

tFloat :: Parser (I TFloat)
tFloat = dec "FLOAT" $ do
  f <- signed pEmpty scientific
  return (TFloat {f = f})

data THex = THex {h :: Integer} deriving (Data)

tHex :: Parser (I THex)
tHex = dec "HEX" $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return THex {h = s}

data TName = TName {n :: Text} deriving (Data)

tName :: Parser (I TName)
tName = dec "NAME" $ do
  l1 <- {-debug "name: first letter"-} lowerChar
  l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return TName {n = pack (l1 : l2)}

-- IDK maybe need to allow indentation after eol
newtype TText = TText {t :: Text} deriving (Data, Show)

tText :: Parser (I TText)
tText = dec "TEXT" $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return TText {t = t}

-- TODO function to combine pos
