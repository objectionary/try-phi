{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ParseEOAlt (tProgram, printTree, Position(..)) where

import           Control.Applicative        (Alternative ((<|>)), optional)
import           Control.Monad.Identity
import           Data.Char                  (digitToInt)
import qualified Data.List                  as DL
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            (MonadParsec (lookAhead, takeWhile1P),
                                             Parsec, SourcePos (SourcePos),
                                             choice, count, getSourcePos, many,
                                             manyTill, some, try,
                                             unPos, (<?>))
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
import           Text.Printf                (printf, PrintfType, IsChar)
import Data.Data (Data(toConstr))
import qualified Data.Text as Text

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
  { row    :: Int,
    column :: Int
  } deriving (Data)

instance Show Position where
  show (Position r c) = printf "%d:%d" r c


data Segment = Segment {start::Position, end::Position} deriving (Data)
instance Show Segment where
  show Segment {..} = printf "[%s..%s]" (show start) (show end)

data Node a b = Node {pos::Segment, node::a, load::b} deriving (Data)
type I a = Node a Load

data Load = Load {id :: Int} | AnotherLoad {k::String, o::Int} deriving (Data)
initLoad = Load {id = 0}

instance Show Load where
  show Load {..} = "(" <> show id <> ")"




-- Printing helpers

tab :: String
tab = "|  "

type TabNumber = Int

tabs :: Int -> String
tabs n = DL.intercalate "" (replicate n tab)

cName :: Data a => a -> String
cName n = show $ toConstr n


showHead :: (PrintfType t, Data a) => Int -> I a -> t
showHead n Node {..} = printf "%s%s %s %s" (tabs n) (cName node) (show pos) (show load)

nothing :: String
nothing = "Nothing"

printTree :: I TProgram -> String
printTree p = printProgram 0 p

-- | for non-leaf nodes
printNonLeaf :: (Data a, Foldable t, Functor t) =>
  Int -> I a -> t (Int -> [Char]) -> [Char]
printNonLeaf n i l = (showHead n i) <> "\n" <> printList ($ n + 1) l

-- | for leaf nodes
printLeaf :: (Show t, Data a) =>
  Int -> I a -> t -> [Char]
printLeaf n i d = showHead n i <>  " " <> (show d) <> "\n"

printMaybe :: (Int -> a -> String) -> Int -> Maybe a -> String
printMaybe f k a = maybe (printNothing k) (f k) a

printList :: (Foldable t, Functor t) => (a1 -> [a2]) -> t a1 -> [a2]
printList f t = foldl (<>) [] (f <$> t)






-- Node printers
instance Show (I TProgram) where
  show t = printProgram 0 t

printProgram :: TabNumber -> I TProgram -> String
printProgram n i@Node {node = TProgram {..}} = printNonLeaf n i [l', m', o']
  where
    l' k = printMaybe printLicense k l
    m' k = printMaybe printMetas k m
    o' k = printObjects k o

instance Show (I TLicense) where
  show t = printLicense 0 t

printLicense :: Int -> I TLicense -> String
printLicense n i@Node {node = TLicense {..}} = printNonLeaf n i [cs']
  where
    cs' k = foldl (<>) [] (printComment k <$> cs)

instance Show (I TComment) where
  show t = printComment 0 t

printComment :: Int -> I TComment -> String
printComment n i@Node {node = TComment {..}} = printLeaf n i c

instance Show (I TMetas) where
  show t = printMetas 0 t

printMetas :: Int -> I TMetas -> String
printMetas n i@Node {node = TMetas {..}} = printNonLeaf n i [cs']
  where
    cs' k = foldl (<>) [] (printMeta k <$> ms)

printNothing :: Int -> String
printNothing n = tabs n <> nothing <> "\n"

instance Show (I TMeta) where
  show t = printMeta 0 t

printMeta :: Int -> I TMeta -> String
printMeta n i@Node {node = TMeta {..}} = printNonLeaf n i [name', suff']
  where
    name' k = printName k name
    suff' k = printMaybe printMetaSuffix k suff

instance Show (I TMetaSuffix) where
  show t = printMetaSuffix 0 t

printMetaSuffix :: Int -> I TMetaSuffix -> String
printMetaSuffix n i@Node {node = TMetaSuffix {..}} = printLeaf n i s

instance Show (I TName) where
  show t = printName 0 t

printName :: Int -> I TName -> String
printName m i@Node {node = TName {..}} = printLeaf m i n

instance Show (I TObjects) where
  show t = printObjects 0 t

printObjects :: Int -> I TObjects -> String
printObjects n i@Node {node = TObjects {..}} = printNonLeaf n i [os']
  where
    os' k = foldl (<>) [] (printObject k <$> os)

instance Show (I TObject) where
  show t = printObject 0 t

printObject :: Int -> I TObject -> String
printObject n i@Node {node = TObject {..}} = printNonLeaf n i [cs', a', t']
  where
    cs' k = foldl (<>) [] (printComment k <$> cs)
    a' k = 
      case a of
        Opt2A p -> printAbstraction k p
        Opt2B p -> printApplication k p
    t' k = printMaybe printTail k t

instance Show (I TAbstraction) where
  show t = printAbstraction 0 t

printAbstraction :: Int -> I TAbstraction -> String
printAbstraction m i@Node {node = TAbstraction {..}} = printNonLeaf m i [as', t']
  where
    as' k = printAttributes k as
    t' k = printMaybe printAbstractionTail k t

instance Show (I TTail) where
  show t = printTail 0 t

printTail :: Int -> I TTail -> String
printTail m i@Node {node = TTail {..}} = printNonLeaf m i [os']
  where
    os' k = foldl (<>) [] (printObject k <$> os)

instance Show (I TApplication) where
  show t = printApplication 0 t

printApplication :: Int -> I TApplication -> String
printApplication m i@Node {node = TApplication {..}} = printNonLeaf m i [s', h', a1']
  where
    s' k = case s of
      Opt2A a -> printHead k a
      Opt2B a -> printApplication k a
    h' k = printMaybe printHtail k h
    a1' k = printApplication1 k a1

instance Show (I TApplication1) where
  show t = printApplication1 0 t

printApplication1 :: Int -> I TApplication1 -> String
printApplication1 m i@Node {node = TApplication1 {..}} = printNonLeaf m i [c']
  where
    c' k = printMaybe printApplication1Elem k c

instance Show (I TApplication1Elem) where
  show t = printApplication1Elem 0 t

printApplication1Elem :: Int -> I TApplication1Elem -> String
printApplication1Elem m i@Node {node = TApplication1Elem {..}} = printNonLeaf m i [c1', ht', a']
  where
    c1' k = case c1 of
      Opt3A t -> printMethod k t
      Opt3B t -> printHas k t
      Opt3C t -> printSuffix k t
    ht' k = printMaybe printHtail k ht
    a' k = printApplication1 k a

instance Show (I TMethod) where
  show t = printMethod 0 t

printMethod :: Int -> I TMethod -> String
printMethod n i@Node {node = TMethod {..}} = printNonLeaf n i [m']
  where
    m' k = case m of
      Opt2A t -> printName k t
      Opt2B t -> printTerminal k t

instance Show (I THas) where
  show t = printHas 0 t

printHas :: Int -> I THas -> String
printHas m i@Node {node = THas {..}} = printNonLeaf m i [n']
  where
    n' k = printName k n

instance Show (I TAttributes) where
  show t = printAttributes 0 t

printAttributes :: Int -> I TAttributes -> String
printAttributes m i@Node {node = TAttributes {..}} = printNonLeaf m i [as']
  where 
    as' k = foldl (<>) [] (printLabel k <$> as)

instance Show (I TAbstractionTail) where
  show t = printAbstractionTail 0 t

printAbstractionTail :: Int -> I TAbstractionTail -> String
printAbstractionTail m i@Node {node = TAbstractionTail {..}} = printNonLeaf m i [e']
  where 
    e' k = 
      case e of
        Opt2A (a,b) -> 
          printSuffix k a <> (
                case b of 
                  Just b' -> 
                    case b' of
                      Opt2A name -> printName k name
                      Opt2B t -> printTerminal k t
                  Nothing -> printNothing k
              )
        Opt2B h -> printHtail k h

instance Show (I THtail) where
  show t = printHtail 0 t

printHtail :: Int -> I THtail -> String
printHtail m i@Node {node = THtail {..}} = printNonLeaf m i [t']
  where
    t' k = foldl (<>) [] (f k <$> t)
    f k e = 
      case e of 
        Opt3A h -> printHead k h
        Opt3B a -> printApplication k a
        Opt3C a -> printAbstraction k a

instance Show (I TLabel) where
  show t = printLabel 0 t

printLabel :: Int -> I TLabel -> String
printLabel m i@Node {node = TLabel {..}} = printNonLeaf m i [l']
  where
    l' k = 
      case l of
        Opt2A t -> printTerminal k t
        Opt2B (n, t) -> 
          printName k n <> printMaybe printTerminal k t

instance Show (I TSuffix) where
  show t = printSuffix 0 t

printSuffix :: Int -> I TSuffix -> String
printSuffix m i@Node {node = TSuffix {..}} = printNonLeaf m i [l', c']
  where 
    l' k = printLabel k l
    c' k = printMaybe printTerminal k c

-- TODO
printTerminal :: Int -> I TTerminal -> String
printTerminal m i@Node {node = t} = printLeaf m i t

instance Show (I THead) where
  show t = printHead 0 t

printHead :: Int -> I THead -> String
printHead m i@Node {node = THead {..}} = printNonLeaf m i [dots', t']
  where 
    dots' k = printMaybe printTerminal k dots
    t' k = 
      case t of
        Opt3A a -> printTerminal k a
        Opt3B a -> printHeadName k a
        Opt3C a -> printData k a 

instance Show (I THeadName) where
  show t = printHeadName 0 t

printHeadName :: Int -> I THeadName -> String
printHeadName m i@Node {node = THeadName {..}} = printNonLeaf m i [name', c']
  where 
    name' k = printName k name
    c' k =
      case c of 
        Opt2A t -> printTerminal k t
        Opt2B t -> printMaybe printTerminal k t 

instance Show (I TData) where
  show t = printData 0 t

printData :: Int -> I TData -> String
printData m i@Node {node = TData {..}} = printNonLeaf m i [d']
  where 
    d' k = 
      case d of
        Opt9A a -> printBool k a
        Opt9B a -> printText k a
        Opt9C a -> printHex k a
        Opt9D a -> printString k a
        Opt9E a -> printFloat k a
        Opt9F a -> printInt k a
        Opt9G a -> printBytes k a
        Opt9H a -> printChar1 k a
        Opt9I a -> printRegex k a


instance Show (I TBool) where
  show t = printBool 0 t

printBool :: Int -> I TBool -> String
printBool m i@Node {node = TBool {..}} = printLeaf m i b

instance Show (I TText) where
  show t = printText 0 t

printText :: Int -> I TText -> String
printText m i@Node {node = TText {..}} = printLeaf m i t

instance Show (I THex) where
  show t = printHex 0 t

printHex :: Int -> I THex -> String
printHex m i@Node {node = THex {..}} = printLeaf m i h

instance Show (I TString) where
  show t = printString 0 t

printString :: Int -> I TString -> String
printString m i@Node {node = TString {..}} = printLeaf m i s

instance Show (I TFloat) where
  show t = printFloat 0 t

printFloat :: Int -> I TFloat -> String
printFloat m i@Node {node = TFloat {..}} = printLeaf m i f

instance Show (I TInt) where
  show t = printInt 0 t

printInt :: Int -> I TInt -> String
printInt m i@Node {node = TInt {..}} = printLeaf m i s

instance Show (I TBytes) where
  show t = printBytes 0 t

printBytes :: Int -> I TBytes -> String
printBytes m i@Node {node = TBytes {..}} = printNonLeaf m i [bs']
  where
    bs' k = 
      case bs of
        Opt3A t -> printTerminal k t
        Opt3B t -> printByte k t
        Opt3C t -> printList (printLineBytes k) t

instance Show (I TChar) where
  show t = printChar1 0 t

printChar1 :: Int -> I TChar -> String
printChar1 m i@Node {node = TChar {..}} = printLeaf m i c

instance Show (I TRegex) where
  show t = printRegex 0 t

printRegex :: Int -> I TRegex -> String
printRegex m i@Node {node = TRegex {..}} = printLeaf m i (r <> " " <> suff)

instance Show (I TLineBytes) where
  show t = printLineBytes 0 t

printLineBytes :: Int -> I TLineBytes -> String
printLineBytes m i@Node {node = TLineBytes {..}} = printNonLeaf m i [bs']
  where
    bs' k = printList (printByte k) bs

instance Show (I TByte) where
  show t = printByte 0 t

printByte :: Int -> I TByte -> String
printByte m i@Node {node = TByte {..}} = printLeaf m i b








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

tEOL :: Parser ()
tEOL = try $ do
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

getIndent :: I TIndent -> Int
getIndent Node {node = TIndent {..}} = n

-- | does common actions for all nodes
dec::Text -> Parser a -> Parser (I a)
dec t p = do
  p1 <- getPos
  p' <- p
  p2 <- getPos
  let ans = Node (Segment p1 p2) p' initLoad
  return ans


-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************



data Options2 a b = Opt2A a | Opt2B b deriving (Data)
data Options3 a b c = Opt3A a | Opt3B b | Opt3C c deriving (Data)

data TProgram = TProgram {l::Maybe (I TLicense), m::Maybe (I TMetas), o::I TObjects} deriving (Data)
tProgram :: Parser (I TProgram)
tProgram = dec "Program" $ do
  l <- optional $ try ({-debug "program:license"-} tLicense)
  m <- optional $ try ({-debug "program:metas"-} tMetas)
  o <- {-debug "program:objects"-} tObjects
  return TProgram {l = l, m = m, o = o}


data TLicense = TLicense {cs::[I TComment]} deriving (Data)
tLicense :: Parser (I TLicense)
tLicense = dec "License" $ do
  cs <- someTry (tComment <* tEOL)
  return TLicense {cs = cs}

data TMetas = TMetas {ms::[I TMeta]} deriving (Data)
tMetas :: Parser (I TMetas)
tMetas = dec "Metas" $ do
  ms <- someTry (tMeta <* tEOL)
  return TMetas {ms = ms}

data TObjects = TObjects {os::[I TObject]} deriving (Data)
tObjects :: Parser (I TObjects)
tObjects = dec "Objects" $ do
  os <- someTry $ tObject noIndent <* tEOL
  return TObjects {os = os}


data TObject = TObject {
  cs::[I TComment],
  a::Options2 (I TAbstraction) (I TApplication),
  t::Maybe (I TTail),
  s::[(I TMethod, Maybe (I THtail), Maybe (I TSuffix), Maybe (I TTail))]} deriving (Data)

tObject :: Int -> Parser (I TObject)
tObject ind = dec "Object" $ do
  comments <- manyTry $ do
    c <- {-debug "object:comment"-} tComment
    e <- tEOLTabMany
    guard $ getIndent e == ind
    return c
  a <-
    choiceTry
      [ Opt2A <$>{-debug "object:abstraction"-} tAbstraction,
        Opt2B <$> {-debug "object:application"-} tApplication
      ]
  let newIndent = ind + indentAdd
  -- list of attributes
  t <- optional $ try ({-debug "object:tail"-} tTail newIndent)
  let g = do
        e <- tEOLTabMany
        guard $ getIndent e == ind
        method <- {-debug "object:method"-} tMethod
        h <- optional $ try ({-debug "object:htail"-} tHtail)
        suffix <- optional $ try ({-debug "object:suffix"-} tSuffix)
        p <- optional $ try ({-debug "object:tail"-} tTail (ind + indentAdd))
        return (method, h, suffix, p)
  s <- manyTry $ ({-debug "object:after tail"-} g)
  return TObject {cs = comments, a = a, t = t, s = s}


data TAbstraction = TAbstraction {as::I TAttributes, t::Maybe (I TAbstractionTail)} deriving (Data)
tAbstraction ::Parser (I TAbstraction)
tAbstraction = dec "Abstraction" $ do
  attrs <- tAttributes
  t <- optional $ try tAbstractionTail
  return TAbstraction {as = attrs, t = t}

data TAbstractionTail = TAbstractionTail {e::Options2 (I TSuffix, Maybe (Options2 (I TName) (I TTerminal))) (I THtail)} deriving (Data)
tAbstractionTail :: Parser (I TAbstractionTail)
tAbstractionTail = dec "Abstraction tail" $ do
  let a = do
      suff <- tSuffix
      o <- optional $ try (
        string cSPACE
        *> string cSLASH
        *> choiceTry
          [ Opt2A <$> {-debug "abstraction:name"-} tName,
            Opt2B <$> tTerminal cQUESTION Question
          ])
      return (suff, o)
  let b = tHtail
  e <- choiceTry [Opt2A <$> a, Opt2B <$> b]
  return TAbstractionTail {e = e}

-- | contains list of arguments
--
-- If no arguments are provided, the list is empty
-- This is the same as making the part between [] optional
data TAttributes = TAttributes {as::[I TLabel]} deriving (Data)
tAttributes :: Parser (I TAttributes)
tAttributes = dec "Attributes" $ do
  _ <- string cLSQ
  attrs <- choiceTry
        [ do
            a <- {-debug "attributes:attribute1"-} tLabel
            as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} tLabel)
            return (a : as),
          [] <$ pEmpty
        ]
  _ <- string cRSQ
  return TAttributes {as = attrs}

data TLabel = TLabel {l::Options2 (I TTerminal) (I TName, Maybe (I TTerminal))} deriving (Data)
tLabel :: Parser (I TLabel)
tLabel = dec "Label" $ do
  l <-
    choiceTry
      [ Opt2A <$> {-debug "label:@"-} (tTerminal cAT At),
        Opt2B <$> (do
          name <- {-debug "label:name"-} tName
          -- TODO move dots to abstraction end (before csq)
          dots <- optional ({-debug "label:..."-} (tTerminal cDOTS Dots))
          return (name, dots))
      ]
  return TLabel {l = l}

data TTail = TTail {os::[I TObject]} deriving (Data)
tTail :: Int -> Parser (I TTail)
tTail ind = dec "Tail" $ do
  let tObj = do
        e <- {-debug "tail:eol"-} tEOLTabMany
        let ind1 = getIndent e
        guard $ ind1 == ind
        tObject ind1
  objects <- someTry tObj
  return TTail {os = objects}

data TSuffix = TSuffix {l::I TLabel, c::Maybe (I TTerminal)} deriving (Data)
tSuffix :: Parser (I TSuffix)
tSuffix = dec "Suffix" $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} tLabel
  c <- optional ({-debug "suffix:const"-} (tTerminal cCONST Const))
  return TSuffix {l = label, c = c}

data TMethod = TMethod {m::Options2 (I TName) (I TTerminal)} deriving (Data)
tMethod :: Parser (I TMethod)
tMethod = dec "Method" $ do
  method <-
    string cDOT
      *> choiceTry
        [ Opt2A <$> {-debug "method:name"-} tName,
          Opt2B <$> {-debug "method:^"-} (tTerminal cRHO Rho),
          Opt2B <$> {-debug "method:@"-} (tTerminal cAT At),
          Opt2B <$> {-debug "method:<"-} (tTerminal cVERTEX Vertex)
        ]
  return TMethod {m = method}

data TApplication = TApplication {s::Options2 (I THead) (I TApplication), h::Maybe (I THtail), a1::I TApplication1} deriving (Data)
tApplication :: Parser (I TApplication)
tApplication = dec "Application" $ do
  s <-
    choiceTry
      [ Opt2A <$>{-debug "application:head"-} tHead,
        Opt2B <$> (string cLB *> {-debug "application:application"-} tApplication <* string cRB)
      ]
  h <- optional $ try ({-debug "application:htail"-} tHtail)
  a1 <- {-debug "application:application1"-} tApplication1
  return TApplication {s = s, h = h, a1 = a1}

data TApplication1 = TApplication1 {c::Maybe (I TApplication1Elem)} deriving (Data)
tApplication1 :: Parser (I TApplication1)
tApplication1 = dec "Application1" $ do
  c <- optional $ try tApplication1Elem
  return TApplication1 {c = c}

data TApplication1Elem = TApplication1Elem {c1::Options3 (I TMethod) (I THas) (I TSuffix), ht::Maybe (I THtail), a::I TApplication1} deriving (Data)
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


data THtail = THtail {t::[Options3 (I THead) (I TApplication) (I TAbstraction)]} deriving (Data)
tHtail :: Parser (I THtail)
tHtail = dec "Htail" $ do
  let op = choiceTry
            [ Opt3A <$> {-debug "htail:head"-} tHead,
              Opt3B <$> (string cLB *> {-debug "htail:application1"-} tApplication <* string cRB),
              Opt3C <$> {-debug "htail:abstraction"-} tAbstraction
            ]
  t <- someTry (string cSPACE *> op)
  return THtail {t = t}

data Options8 a b c d e f g h =
    Options8A a
  | Options8B b
  | Options8C c
  | Options8D d
  | Options8E e
  | Options8F f
  | Options8G g
  | Options8H h
  deriving (Data)

data TTerminal =
    Root
  | Xi
  | Sigma
  | Dot
  | Copy
  | Star
  | At
  | Rho
  | Vertex
  | EmptyBytes
  | Question
  | Dots
  | Const
  deriving (Data, Show)

tTerminal :: Text -> TTerminal -> Parser (I TTerminal)
tTerminal s t = dec s $ do
  p1 <- getPos
  void (string s)
  p2 <- getPos
  return t

data THead = THead {dots::Maybe (I TTerminal), t::Options3 (I TTerminal) (I THeadName) (I TData)} deriving (Data)
tHead :: Parser (I THead)
tHead = dec "Head" $ do
  dots <- optional $ tTerminal cDOTS Dots
  t <- choiceTry
      [ Opt3A <$>{-debug "head:root"-}  (tTerminal cROOT Root),
        Opt3A <$>{-debug "head:at"-}  (tTerminal cAT At),
        Opt3A <$>{-debug "head:rho"-}  (tTerminal cRHO Rho),
        Opt3A <$>{-debug "head:xi"-}   (tTerminal cXI Xi),
        Opt3A <$>{-debug "head:sigma"-}   (tTerminal cSIGMA Sigma),
        Opt3A <$>{-debug "head:star"-}   (tTerminal cSTAR Star),
        Opt3B <$> tHeadName,
        Opt3C <$>{-debug "head:data"-} (tData)
      ]
  return THead {dots = dots, t = t}

-- TODO lookahead EOL
data THeadName = THeadName {name::I TName, c::Options2 (I TTerminal) (Maybe (I TTerminal))} deriving (Data)
tHeadName :: Parser (I THeadName)
tHeadName = dec "Head name" $ do
  name <- tName
  c <- choiceTry [
        Opt2A <$> tTerminal cDOT Dot <* lookAhead (string cSPACE)
      , Opt2B <$> optional (tTerminal cCOPY Copy)
      ]
  return THeadName {name = name, c = c}


data THas = THas {n::I TName} deriving (Data)
tHas :: Parser (I THas)
tHas = dec "Has" $ do
  _ <- string cCOLON
  n <- {-debug "has:name"-} tName
  return THas {n = n}


data Options9 a b c d e f g h i =
    Opt9A a
  | Opt9B b
  | Opt9C c
  | Opt9D d
  | Opt9E e
  | Opt9F f
  | Opt9G g
  | Opt9H h
  | Opt9I i
  deriving (Data)

data TData = TData {d::Options9 (I TBool) (I TText) (I THex) (I TString) (I TFloat) (I TInt) (I TBytes) (I TChar) (I TRegex)} deriving (Data)
tData :: Parser (I TData)
tData = dec "DATA" $ do
  d <-
    choiceTry
      [ {-debug "data:bool"-}Opt9A <$> tBool,
        {-debug "data:text"-} Opt9B <$>tText,
        {-debug "data:hex"-} Opt9C <$>tHex,
        {-debug "data:string"-} Opt9D <$>tString,
        {-debug "data:float"-} Opt9E <$>tFloat,
        {-debug "data:int"-} Opt9F <$>tInt,
        {-debug "data:bytes"-} Opt9G <$>tBytes,
        {-debug "data:char"-} Opt9H <$>tChar,
        {-debug "data:regex"-} Opt9I <$>tRegex
      ]
  return TData {d = d}

data TComment = TComment {c::Text} deriving (Data)
tComment :: Parser (I TComment)
tComment = dec "COMMENT" $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return TComment {c = content}


data TMeta = TMeta {name::I TName, suff::Maybe (I TMetaSuffix)} deriving (Data)
tMeta :: Parser (I TMeta)
tMeta = dec "META" $ do
  _ <- string cPLUS
  name <- {-debug "meta:name"-} tName
  suffix <- {-debug "meta:suffix"-} (optional $ try tMetaSuffix)
  return TMeta {name = name, suff = suffix}

data TMetaSuffix = TMetaSuffix {s::Text} deriving (Data)
tMetaSuffix :: Parser (I TMetaSuffix)
tMetaSuffix = dec "Meta Suffix" $ do
  suffix <- {-debug "meta:suffix"-} (pack <$> (string cSPACE *> some printChar))
  return TMetaSuffix {s = suffix}


data TRegex = TRegex {r :: Text, suff :: Text} deriving (Data)
tRegex :: Parser (I TRegex)
tRegex = dec "REGEX" $ do
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  return (TRegex {r = r, suff = suffix})


data TIndent = TIndent {n::Int} deriving (Data)
tEOLTabMany :: Parser (I TIndent)
tEOLTabMany = dec "EOL_TAB_MANY" $ do
  _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return TIndent {n = nIndents}

data TByte = TByte {b :: Integer} deriving (Data)
tByte :: Parser (I TByte)
tByte = dec "BYTE" $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return TByte {b = b}

data TLineBytes = TLineBytes {bs::[I TByte]} deriving (Data)
tLineBytes :: Parser (I TLineBytes)
tLineBytes = dec "LINE_BYTES" $ do
  byte <- {-debug "line_bytes:byte"-} tByte
  bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> tByte))
  return TLineBytes {bs = byte : bytes}

data TBytes = TBytes {bs::Options3 (I TTerminal) (I TByte) [I TLineBytes]} deriving (Data)
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
      s <- tTerminal cEMPTY_BYTES EmptyBytes
      return (Opt3A s)
    parser2 = do
      byte <- tByte
      _ <- string cMINUS
      return (Opt3B byte)
    parser4 = do
      _ <- string cMINUS
      -- TODO guard indentation
      e <- tEOLTabMany
      lb <- tLineBytes
      return lb
    parser3 = do
      lb <- tLineBytes
      lbs <- manyTry parser4
      return (Opt3C (lb : lbs))

data TBool = TBool {b::Bool} deriving (Data)
tBool :: Parser (I TBool)
tBool = dec "BOOL" $ do
  b <-
    choiceTry
      [ True <$ string cTRUE,
        False <$ string cFALSE
      ]
  return TBool {b = b}

data TChar = TChar {c::Char} deriving (Data)
-- | slightly differs from grammar: doesn't allow u Byte Byte
tChar :: Parser (I TChar)
tChar = dec "CHAR" $ do
  c <- char '\'' *> charLiteral <* char '\''
  return TChar {c = c}


data TString = TString {s::Text} deriving (Data)
-- | slightly differs from grammar: doesn't allow u Byte Byte
tString :: Parser (I TString)
tString = dec "STRING" $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return TString {s = s}


data TInt = TInt {s::Integer} deriving (Data)
tInt :: Parser (I TInt)
tInt = dec "INT" $ do
  s <- signed pEmpty decimal
  return TInt {s = s}


data TFloat = TFloat {f::Scientific} deriving (Data)
tFloat :: Parser (I TFloat)
tFloat = dec "FLOAT" $ do
  f <- signed pEmpty scientific
  return (TFloat {f = f})

data THex = THex {h::Integer} deriving (Data)
tHex :: Parser (I THex)
tHex = dec "HEX" $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return THex {h = s}


data TName = TName {n::Text} deriving (Data)
tName :: Parser (I TName)
tName = dec "NAME" $ do
  l1 <- {-debug "name: first letter"-} lowerChar
  l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return TName {n = pack (l1:l2)}

-- IDK maybe need to allow indentation after eol
newtype TText = TText {t::Text} deriving (Data)
tText :: Parser (I TText)
tText = dec "TEXT" $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return TText {t = t}

-- TODO function to combine pos