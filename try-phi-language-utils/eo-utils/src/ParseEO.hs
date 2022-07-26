{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ParseEO
  ( tProgram,
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
    THeadTerminal (..),
    TMethodTerminal (..),
    TLabelTerminal (..),
    THeadModifier (..),
    TAbstrQuestion (..),
    TRegexBody (..),
    TRegexSuffix (..),
    TObjectTail (..),
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
    cQUESTION,
    EpiAnn(..), Segment(..), Ann(..)
  )
where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad.Identity (guard)
import Data.Char (digitToInt)
import Data.Data (Data)
import Data.Scientific (Scientific)
import Data.Text (Text, pack)
import qualified Data.Text as T

import Data.Void (Void)
import ParseEOTH
    ( EpiAnn(..),
      Segment(Segment, start, end),
      Ann(Ann, num, segment),
      Position(..),
      genEpiN )
import Text.Megaparsec
  ( MonadParsec (lookAhead, notFollowedBy, takeWhile1P),
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
import Text.Megaparsec.Debug (dbg)
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

-- Tokens

data Options3 a b c = Opt3A a | Opt3B b | Opt3C c deriving (Data, Show)

data Options2 a b = Opt2A a | Opt2B b deriving (Data, Show)

data TProgram = TProgram {l :: Maybe TLicense, m :: Maybe TMetas, o :: TObjects, ann :: Ann} deriving (Data)

data TLicense = TLicense {cs :: [TComment], ann :: Ann} deriving (Data)

data TMetas = TMetas {ms :: [TMeta], ann :: Ann} deriving (Data)

data TObjects = TObjects {os :: [TObject], ann :: Ann} deriving (Data)

data TObjectTail = TObjectTail {m :: TMethod, h :: Maybe THtail, s :: Maybe TSuffix, t :: Maybe TTail, ann :: Ann} deriving (Data)

data TObject = TObject
  { ann :: Ann,
    cs :: [TComment],
    a :: Options2 TAbstraction TApplication,
    t :: Maybe TTail,
    s :: [TObjectTail]
  }
  deriving (Data)

data TAbstraction = TAbstraction {as :: TAttributes, t :: Maybe TAbstractionTail, ann :: Ann} deriving (Data)

data TAbstractionTail = TAbstractionTail {e :: Options2 (TSuffix, Maybe (Options2 TName TAbstrQuestion)) THtail, ann :: Ann} deriving (Data)

newtype TAbstrQuestion = TAbstrQuestion {ann :: Ann} deriving (Data)

data TAttributes = TAttributes {as :: [TFreeAttribute], ann :: Ann} deriving (Data)

data TVarArg = TVarArg {n :: Text, ann :: Ann} deriving (Data)

data TFreeAttribute = TFreeAttribute {l :: Options3 TLabelTerminal TName TVarArg, ann :: Ann} deriving (Data)

data TLabel = TLabel {l :: Options2 TLabelTerminal (TName, Maybe TDots), ann :: Ann} deriving (Data)

newtype TLabelTerminal = LabelAt {ann :: Ann} deriving (Data)

data TTail = TTail {os :: [TObject], ann :: Ann} deriving (Data)

data TSuffix = TSuffix {l :: TLabel, c :: Maybe TConst, ann :: Ann} deriving (Data)

data TMethodTerminal
  = TMethodRho {ann :: Ann}
  | TMethodAt {ann :: Ann}
  | TMethodVertex {ann :: Ann}
  deriving (Data)

data TMethod = TMethod {m :: Options2 TName TMethodTerminal, ann :: Ann} deriving (Data)

data TApplication = TApplication {s :: Options2 THead TApplication, h :: Maybe THtail, a1 :: TApplication1, ann :: Ann} deriving (Data)

data TApplication1 = TApplication1 {c :: Maybe TApplication1Elem, ann :: Ann} deriving (Data)

data TApplication1Elem = TApplication1Elem {c1 :: Options3 TMethod THas TSuffix, ht :: Maybe THtail, a :: TApplication1, ann :: Ann} deriving (Data)

data THtail = THtail {t :: [Options3 THead TApplication TAbstraction], ann :: Ann} deriving (Data)

newtype TDots = TDots {ann :: Ann} deriving (Data)

newtype TConst = TConst {ann :: Ann} deriving (Data)

-- TODO prohibit dots before data
data THead = THead {dots :: Maybe TDots, t :: Options3 THeadTerminal THeadName TData, ann :: Ann} deriving (Data)

data THeadTerminal
  = THeadRoot {ann :: Ann}
  | THeadAt {ann :: Ann}
  | THeadRho {ann :: Ann}
  | THeadXi {ann :: Ann}
  | THeadSigma {ann :: Ann}
  | THeadStar {ann :: Ann}
  deriving (Data)

data THeadName = THeadName {name :: TName, c :: Maybe THeadModifier, ann :: Ann} deriving (Data)

data THeadModifier
  = THeadDot {ann :: Ann}
  | THeadCopy {ann :: Ann}
  deriving (Data)

data THas = THas {n :: TName, ann :: Ann} deriving (Data)

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

data TData = TData {d :: Options9 TBool TText THex TString TFloat TInt TBytes TChar TRegex, ann :: Ann} deriving (Data)

data TRegex = TRegex {rb :: TRegexBody, rs :: TRegexSuffix, ann :: Ann} deriving (Data)

data TIndent = TIndent {n :: Int, ann :: Ann} deriving (Data)

data TByte = TByte {b :: Integer, ann :: Ann} | EmptyBytes deriving (Data)

data TLineBytes = TLineBytes {bs :: [TByte], ann :: Ann} deriving (Data)

data TBytes = TBytes {bs :: Options2 TByte [TLineBytes], ann :: Ann} deriving (Data)

data TBool = TBool {b :: Bool, ann :: Ann} deriving (Data)

data TChar = TChar {c :: Char, ann :: Ann} deriving (Data)

data TString = TString {s :: Text, ann :: Ann} deriving (Data)

data TInt = TInt {i :: Integer, ann :: Ann} deriving (Data)

data TFloat = TFloat {f :: Scientific, ann :: Ann} deriving (Data)

data THex = THex {h :: Integer, ann :: Ann} deriving (Data)

data TName = TName {n :: Text, ann :: Ann} deriving (Data)

data TText = TText {t :: Text, ann :: Ann} deriving (Data)

data TComment = TComment {c :: Text, ann :: Ann} deriving (Data)

data TMeta = TMeta {name :: TName, suff :: Maybe TMetaSuffix, ann :: Ann} deriving (Data)

data TMetaSuffix = TMetaSuffix {s :: Text, ann :: Ann} deriving (Data)

data TRegexBody = TRegexBody {b :: Text, ann :: Ann} deriving (Data)

data TRegexSuffix = TRegexSuffix {s :: Text, ann :: Ann} deriving (Data)

$( genEpiN
     [ ''TAbstrQuestion,
       ''TAbstraction,
       ''TAbstractionTail,
       ''TApplication,
       ''TApplication1,
       ''TApplication1Elem,
       ''TAttributes,
       ''TBool,
       ''TByte,
       ''TBytes,
       ''TChar,
       ''TComment,
       ''TConst,
       ''TData,
       ''TDots,
       ''TFloat,
       ''TFreeAttribute,
       ''THas,
       ''THead,
       ''THeadModifier,
       ''THeadName,
       ''THeadTerminal,
       ''THex,
       ''THtail,
       ''TIndent,
       ''TInt,
       ''TLabel,
       ''TLabelTerminal,
       ''TLicense,
       ''TLineBytes,
       ''TMeta,
       ''TMetaSuffix,
       ''TMetas,
       ''TMethod,
       ''TMethodTerminal,
       ''TName,
       ''TObject,
       ''TObjectTail,
       ''TObjects,
       ''TProgram,
       ''TRegex,
       ''TRegexBody,
       ''TRegexSuffix,
       ''TString,
       ''TSuffix,
       ''TTail,
       ''TText,
       ''TVarArg
     ]
 )

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
hexToInt = foldl (\x acc -> acc * 16 + x) 0

debugFlag :: Bool
debugFlag = True

data DebugMode = On | Off

debug label parser
  | debugFlag = dbg label parser
  | otherwise = parser <?> label

manyTry :: MonadParsec e s m => m a -> m [a]
manyTry p = try $ many (try p)

someTry :: MonadParsec e s m => m a -> m [a]
someTry p = try $ some (try p)

choiceTry :: MonadParsec e s m => [m a] -> m a
choiceTry p = try $ choice (map try p)

-- | print a debug message when enter a node
enter :: Show a => a -> Parser ()
enter name = do
  pos <- getPos
  debug (show pos <> ": Enter " <> show name) pEmpty
  return ()

tEOL :: Parser ()
tEOL = try $ do
  _ <- eol *> optional (try eol)
  return ()

-- | print a debug message when leave a node
leave :: Show a => a -> p -> Parser ()
leave name node = do
  pos <- getPos
  let l = printf "%s: Leave %s" (show pos) (show name)
  debug l pEmpty
  return ()

noIndent :: Int
noIndent = 0

indentAdd :: Int
indentAdd = 1

getIndent :: TIndent -> Int
getIndent TIndent {..} = n

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************

tProgram :: Parser TProgram
tProgram = dec $ do
  l <- optional $ try {-debug "program:license"-} tLicense
  m <- optional $ try {-debug "program:metas"-} tMetas
  o {-debug "program:objects"-} <- tObjects
  return TProgram {l = l, m = m, o = o}

tLicense :: Parser TLicense
tLicense = dec $ do
  cs <- someTry (tComment <* tEOL)
  return TLicense {cs = cs}

tMetas :: Parser TMetas
tMetas = dec $ do
  ms <- someTry (tMeta <* tEOL)
  return TMetas {ms = ms}

tObjects :: Parser TObjects
tObjects = dec $ do
  os <- someTry $ tObject noIndent <* tEOL
  return TObjects {os = os}

tObject :: Int -> Parser TObject
tObject ind = dec $ do
  comments <- manyTry $ do
    c {-debug "object:comment"-} <- tComment
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
        method {-debug "object:method"-} <- tMethod
        h <- optional $ try {-debug "object:htail"-} tHtail
        suffix <- optional $ try {-debug "object:suffix"-} tSuffix
        p <- optional $ try ({-debug "object:tail"-} tTail (ind + indentAdd))
        dec $ return TObjectTail {m = method, h = h, s = suffix, t = p}
  s <- manyTry {-debug "object:after tail"-} g1
  return TObject {cs = comments, a = a, t = t, s = s}

-- TODO use separate parser for free attributes

tAbstraction :: Parser TAbstraction
tAbstraction = dec $ do
  attrs <- tAttributes
  t <- optional $ try tAbstractionTail
  return TAbstraction {as = attrs, t = t}

-- TODO Use separate terminal for question

tAbstractionTail :: Parser TAbstractionTail
tAbstractionTail = dec $ do
  let a = do
        suff <- tSuffix
        o <-
          optional $
            try
              ( string cSPACE
                  *> string cSLASH
                  *> choiceTry
                    [ Opt2A <$> {-debug "abstraction:name"-} tName,
                      Opt2B <$> tTerminal cQUESTION TAbstrQuestion
                    ]
              )
        return (suff, o)
  let b = tHtail
  e <- choiceTry [Opt2A <$> a, Opt2B <$> b]
  return TAbstractionTail {e = e}

-- | list of arguments
--
-- If no arguments are provided, the list is empty
--
-- This is the same as making the part between [] optional
tAttributes :: Parser TAttributes
tAttributes = dec $ do
  _ <- string cLSQ
  attrs <-
    choiceTry
      [ do
          a {-debug "attributes:attribute1"-} <- tFreeAttribute
          as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} tFreeAttribute)

          -- last argument may be vararg
          d <- optional $ try (tTerminal cDOTS TDots)
          let as' = a : as
          -- FIXME
          let p@TFreeAttribute {..} = last as'
          let l' =
                case d of
                  Just _ ->
                    case l of
                      Opt3A _ -> error "decoratee cannot be vararg"
                      Opt3B n1@TName {..} -> Opt3C (TVarArg {ann = ann, n = n})
                      _ -> error "couldn't have parsed dots in free attributes"
                  _ -> l
          return (init as' ++ [p {l = l'} :: TFreeAttribute]),
        [] <$ pEmpty
      ]
  _ <- string cRSQ
  return TAttributes {as = attrs}

tFreeAttribute :: Parser TFreeAttribute
tFreeAttribute = dec $ do
  l <-
    choiceTry
      [ Opt3A <$> {-debug "label:@"-} tTerminal cAT LabelAt,
        Opt3B <$> {-debug "label:name"-} tName
      ]
  return TFreeAttribute {l = l}

tLabel :: Parser TLabel
tLabel = dec $ do
  l <-
    choiceTry
      [ Opt2A <$> {-debug "label:@"-} tTerminal cAT LabelAt,
        Opt2B
          <$> ( do
                  name {-debug "label:name"-} <- tName
                  -- TODO move dots to abstraction end (before csq)
                  dots <- optional ({-debug "label:..."-} tTerminal cDOTS TDots)
                  return (name, dots)
              )
      ]
  return TLabel {l = l}

tTail :: Int -> Parser TTail
tTail ind = dec $ do
  let tObj = do
        e {-debug "tail:eol"-} <- tEOLTabMany
        let ind1 = getIndent e
        guard $ ind1 == ind
        tObject ind1
  objects <- someTry tObj
  return TTail {os = objects}

tSuffix :: Parser TSuffix
tSuffix = dec $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} tLabel
  c <- optional ({-debug "suffix:const"-} tTerminal cCONST TConst)
  return TSuffix {l = label, c = c}

-- TODO separate set of terminals for Method

tMethod :: Parser TMethod
tMethod = dec $ do
  method <-
    string cDOT
      *> choiceTry
        [ Opt2A <$> {-debug "method:name"-} tName,
          Opt2B <$> {-debug "method:^"-} tTerminal cRHO TMethodRho,
          Opt2B <$> {-debug "method:@"-} tTerminal cAT TMethodAt,
          Opt2B <$> {-debug "method:<"-} tTerminal cVERTEX TMethodVertex
        ]
  return TMethod {m = method}

tApplication :: Parser TApplication
tApplication = dec $ do
  s <-
    choiceTry
      [ Opt2A <$> {-debug "application:head"-} tHead,
        Opt2B <$> (string cLB *> {-debug "application:application"-} tApplication <* string cRB)
      ]
  h <- optional $ try {-debug "application:htail"-} tHtail
  a1 {-debug "application:application1"-} <- tApplication1
  return TApplication {s = s, h = h, a1 = a1}

-- | appeared after fixing left recursion
tApplication1 :: Parser TApplication1
tApplication1 = dec $ do
  -- this rule can read an empty string, that's why here stands 'optional'
  c <- optional $ try tApplication1Elem
  return TApplication1 {c = c}

tApplication1Elem :: Parser TApplication1Elem
tApplication1Elem = dec $ do
  c1 <-
    choiceTry
      [ Opt3A <$> {-debug "application1:method"-} tMethod,
        Opt3B <$> {-debug "application1:has"-} tHas,
        Opt3C <$> {-debug "application1:suffix"-} tSuffix
      ]
  ht <- optional $ try {-debug "application1:htail"-} tHtail
  a {-debug "application1:application1"-} <- tApplication1
  return TApplication1Elem {c1 = c1, ht = ht, a = a}

-- |
-- -- TODO
--
-- Currently, we don't parse application followed by method, has, or suffix.
--
-- This allows for left associative argument lists
tHtail :: Parser THtail
tHtail = dec $ do
  let op =
        choiceTry
          [ Opt3A <$> {-debug "htail:head"-} tHead,
            Opt3B <$> (string cLB *> {-debug "htail:application1"-} tApplication <* string cRB),
            Opt3C <$> (string cLB *> {-debug "htail:abstraction"-} tAbstraction <* string cRB)
          ]
  t <- someTry (string cSPACE *> op)
  return THtail {t = t}

tTerminal :: EpiAnn a => Text -> (Ann -> a) -> Parser a
tTerminal s t =
  dec $ do
    _ <- string s
    return (t undefined)

{-
>>>runQ [d|instance EpiAnn TDots where setAnn a b = b {ann = a}|]
-}

tHead :: Parser THead
tHead = dec $ do
  dots <- optional $ tTerminal cDOTS TDots
  t <-
    choiceTry
      [ Opt3A <$> {-debug "head:root"-} (tTerminal cROOT THeadRoot),
        Opt3A <$> {-debug "head:at"-} (tTerminal cAT THeadAt),
        Opt3A <$> {-debug "head:rho"-} (tTerminal cRHO THeadRho),
        Opt3A <$> {-debug "head:xi"-} (tTerminal cXI THeadXi),
        Opt3A <$> {-debug "head:sigma"-} (tTerminal cSIGMA THeadSigma),
        Opt3A <$> {-debug "head:star"-} (tTerminal cSTAR THeadStar),
        Opt3B <$> tHeadName,
        Opt3C <$> {-debug "head:data"-} (tData)
      ]
  return THead {dots = dots, t = t}

-- TODO lookahead EOL

-- | head name with a
--
-- dot: a.
--
-- copy: a'
tHeadName :: Parser THeadName
tHeadName = dec $ do
  name <- tName
  c <-
    choiceTry
      [ Just <$> tTerminal cDOT THeadDot <* lookAhead (string cSPACE),
        optional (tTerminal cCOPY THeadCopy)
      ]
  return THeadName {name = name, c = c}

tHas :: Parser THas
tHas = dec $ do
  _ <- string cCOLON
  n {-debug "has:name"-} <- tName
  return THas {n = n}

tData :: Parser TData
tData = dec $ do
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

tComment :: Parser TComment
tComment = dec $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return TComment {c = content}

tMeta :: Parser TMeta
tMeta = dec $ do
  _ <- string cPLUS
  name {-debug "meta:name"-} <- tName
  suffix {-debug "meta:suffix"-} <- (optional $ try tMetaSuffix)
  return TMeta {name = name, suff = suffix}

tMetaSuffix :: Parser TMetaSuffix
tMetaSuffix = dec $ do
  suffix {-debug "meta:suffix"-} <- (pack <$> (string cSPACE *> some printChar))
  return TMetaSuffix {s = suffix}

-- TODO Regex nodes

tRegex :: Parser TRegex
tRegex = dec $ do
  _ <- string cSLASH
  r <- dec $ (\x -> TRegexBody {b = x}) <$> takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- dec $ (\x -> TRegexSuffix {s = x}) . pack <$> many alphaNumChar
  return (TRegex {rb = r, rs = suffix})

tEOLTabMany :: Parser TIndent
tEOLTabMany = dec $ do
  _ {-debug "eol:eol"-} <- (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return TIndent {n = nIndents}

tByte :: Parser TByte
tByte = dec $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return TByte {b = b}

tLineBytes :: Parser TLineBytes
tLineBytes = dec $ do
  byte {-debug "line_bytes:byte"-} <- tByte
  bytes {-debug "line_bytes:bytes"-} <- (someTry (string cMINUS *> tByte))
  return TLineBytes {bs = byte : bytes}

tBytes :: Parser TBytes
tBytes = dec $ do
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
      emp <- dec (EmptyBytes <$ string cEMPTY_BYTES)
      return (Opt2A emp)
    parser2 = do
      byte <- tByte
      _ <- string cMINUS
      return (Opt2A byte)
    parser4 = do
      _ <- string cMINUS
      -- TODO guard indentation
      e <- tEOLTabMany
      tLineBytes
    parser3 = do
      lb <- tLineBytes
      lbs <- manyTry parser4
      return (Opt2B (lb : lbs))

tBool :: Parser TBool
tBool = dec $ do
  b <-
    choiceTry
      [ True <$ string cTRUE,
        False <$ string cFALSE
      ]
  return TBool {b = b}

-- | slightly differs from grammar: doesn't allow u Byte Byte
tChar :: Parser TChar
tChar = dec $ do
  c <- char '\'' *> charLiteral <* char '\''
  return TChar {c = c}

-- | slightly differs from grammar: doesn't allow u Byte Byte
tString :: Parser TString
tString = dec $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return TString {s = s}

tInt :: Parser TInt
tInt = dec $ do
  s <- signed pEmpty decimal
  return TInt {i = s}

tFloat :: Parser TFloat
tFloat = dec $ do
  f <- signed pEmpty scientific
  return (TFloat {f = f})

tHex :: Parser THex
tHex = dec $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return THex {h = s}

tName :: Parser TName
tName = dec $ do
  l1 {-debug "name: first letter"-} <- lowerChar
  l2 {-debug "name: other letters"-} <- (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return TName {n = pack (l1 : l2)}

-- IDK maybe need to allow indentation after eol
tText :: Parser TText
tText = dec $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return TText {t = t}

dec :: (EpiAnn a) => Parser a -> Parser a
dec p = do
  -- enter t
  p1 <- getPos
  p' <- p
  p2 <- getPos
  let ann = Ann {num = 3, segment = Segment {start = p1, end = p2}}
  return (modify (const ann) p')
