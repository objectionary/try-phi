{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module ParseEOAlt (tProgram, Node (..), Position(..), TokenType (..)) where

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
  deriving (Show, Eq)

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

-- printTree :: TabNumber -> Node -> String
-- printTree n Node {..} =
--   DL.intercalate "" (replicate n tab)
--     <> case nodeToken of
--       JustNode -> printf "%s\n" (show nodeToken)
--       ListNode -> printf "%s\n" (show nodeToken)
--       NothingNode -> printf "%s\n" (show nodeToken)
--       _ -> printf "%s [%s..%s]\n" (show nodeToken) (show start) (show end)
--     <> foldl (\s a -> s <> printTree (n + 1) a) "" nodes

instance Show Node where
  show n = printTree 0 n

-- initNode :: Node
-- initNode =
--   Node
--     { nodeToken = NONE,
--       nodes = [],
--       start = Position 0 0,
--       end = Position 0 0
--     }

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

-- pHexDigit :: Char -> Char -> Parser Int
-- pHexDigit l1 l2 = do
--   c <- hexDigitChar
--   guard (c `elem` ['0' .. '9'] <> [l1 .. l2])
--   let c' = digitToInt c
--   return c'

pHexDigitUpper :: Parser Integer
pHexDigitUpper = toInteger <$> pHexDigit 'A' 'F'

pHexDigitLower :: Parser Integer
pHexDigitLower = toInteger <$> pHexDigit 'a' 'f'

pEmpty :: Parser ()
pEmpty = return ()

hexToInt :: [Integer] -> Integer
hexToInt = foldl (\x acc -> (acc * 16) + x) 0

-- pTerminal :: Text -> TokenType -> Parser Node
-- pTerminal s t = do
--   p1 <- getPos
--   void (string s)
--   p2 <- getPos
--   return $ Node t [] p1 p2

-- listNode :: Parser [Node] -> Parser Node
-- listNode p = do
--   ns <- try p
--   return initNode
--     { nodeToken = ListNode,
--       nodes = ns
--     }

-- maybeToNode :: Maybe Node -> Node
-- maybeToNode (Just n) = n
-- maybeToNode Nothing =
--   initNode
--     { nodeToken = NothingNode
--     }

-- | keeps parser if it succeeds
--
-- otherwise, produces a parser with `NothingNode` tag
-- optionalNode :: Parser Node -> Parser Node
-- optionalNode p = try $ do
--   p1 <- getPos
--   n <- maybeToNode <$> optional (try p)
--   p2 <- getPos
--   return (n {start = p1, end = p2} ::Node)

-- textNode :: Parser Text -> Parser Node
-- textNode txt = do
--   p1 <- getPos
--   t <- try txt
--   p2 <- getPos
--   return
--     initNode
--       { nodeToken = TextNode t,
--         start = p1,
--         end = p2
--       }

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
-- dec :: Show a1 => a1 -> Parser (TokenType, [Node]) -> Parser Node
-- dec name p = do
--   p1 <- getPos
--   -- enter name
--   (t,l) <- p
--   p2 <- getPos
--   let ans = Node t l p1 p2
--   -- leave name ans
--   return ans

getIndent :: I TIndent -> Int
getIndent (Info l TIndent {..}) = n
-- getIndent :: Node -> Int
-- getIndent n =
--   case n of
--     Node {nodeToken = INDENT ind} -> ind
--     _                             -> 0

dec1::(Loadable a) => Text -> Parser a -> Parser (I a)
dec1 t p = do
  p1 <- get
  p' <- p
  p2 <- get
  let ans = Info (Load p1 p2) p'
  return ans

class Loadable a where
  getLoad :: a -> Load
  putLoad :: Load -> a -> a

-- can be used to combine positions of child nodes
data I a = Info {l::Load, node::a}

instance Loadable (I a) where
  getLoad Info {..} = l
  putLoad l x = x {l = l}

-- data N =

-- ***************************************************

-- Parsers | Parsers | Parsers | Parsers | Parsers

-- ***************************************************


data Options2 a b = Opt2A a | Opt2B b
data Options3 a b c = Opt3A a | Opt3B b | Opt3C c
data Options4 a b c d = Opt4A a | Opt4B b | Opt4C c | Opt4D

data TProgram = TProgram {l::Maybe (I TLicense), m::Maybe (I TMetas), o::I TObjects}
tProgram :: Parser (I TProgram)
tProgram = dec1 "Program" $ do
  l <- optional $ try ({-debug "program:license"-} tLicense)
  m <- optional $ try ({-debug "program:metas"-} tMetas)
  o <- {-debug "program:objects"-} tObjects
  return TProgram {l = l, m = m, o = o}

-- pProgram :: Parser Node
-- pProgram = dec "Program" $ do
--   l <- optionalNode ({-debug "program:license"-} pLicense)
--   m <- optionalNode ({-debug "program:metas"-} pMetas)
--   o <- {-debug "program:objects"-} pObjects
--   return (Program, [l, m, o])

data TLicense = TLicense {cs::[I TComment]}
tLicense :: Parser (I TLicense)
tLicense = dec1 "License" $ do
  cs <- someTry (tCOMMENT <* tEOL)
  return TLicense {cs = cs}

-- pLicense :: Parser Node
-- pLicense = dec "License" $ do
--   cs <- someTry (pCOMMENT <* pEOL)
--   return (License, cs)

data TMetas = TMetas {ms::[I TMeta]}
tMetas :: Parser (I TMetas)
tMetas = dec1 "Metas" $ do
  ms <- someTry (tMETA <* tEOL)
  return TMetas {ms = ms}

-- pMetas :: Parser Node
-- pMetas = dec "Metas" $ do
--   ms <- someTry (pMETA <* pEOL)
--   return (Metas, ms)

data TObjects = TObjects {os::[I TObject]}
tObjects :: Parser (I TObjects)
tObjects = dec1 "Objects" $ do
  -- os <- someTry $ pObject noIndent <* pEOL_TAB_MANY
  os <- someTry $ tObject noIndent <* tEOL
  return TObjects {os = os}

-- pObjects :: Parser Node
-- pObjects = dec "Objects" $ do
--   -- os <- someTry $ pObject noIndent <* pEOL_TAB_MANY
--   os <- someTry $ pObject noIndent <* pEOL
--   return (Objects, os)


data TObject = TObject {
  cs::[I TComment], 
  a::Options2 (I TAbstraction) (I TApplication), 
  t::Maybe (I TTail), 
  s::[(I TMethod, Maybe (I THtail), Maybe (I TSuffix), Maybe (I TTail))]}

tObject :: Int -> Parser (I TObject)
tObject ind = dec1 "Object" $ do
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


-- pObject :: Int -> Parser Node
-- pObject ind = dec "Object" $ do
--   comments <- listNode $ manyTry $ do
--     c <- {-debug "object:comment"-} pCOMMENT
--     e <- pEOL_TAB_MANY
--     guard $ getIndent e == ind
--     return c
--   a <-
--     choiceTry
--       [ {-debug "object:abstraction"-} pAbstraction,
--         {-debug "object:application"-} pApplication
--       ]
--   let newIndent = ind + indentAdd
--   -- list of attributes
--   t <- optionalNode ({-debug "object:tail"-} pTail newIndent)

--   -- TODO ban suffix 
--   let g = do
--         e <- pEOL_TAB_MANY
--         guard $ getIndent e == ind
--         method <- {-debug "object:method"-} pMethod
--         h <- optionalNode ({-debug "object:htail"-} pHtail)
--         suffix <- optionalNode ({-debug "object:suffix"-} pSuffix)
--         p <- optionalNode ({-debug "object:tail"-} pTail (ind + indentAdd))
--         return [method, h, suffix, p]
--   s <- listNode $ manyTry $ listNode ({-debug "object:after tail"-} g)
--   return (Object, [comments, a, t, s])

data TAbstraction = TAbstraction {as::I TAttributes, t::Maybe (I TAbstractionTail)}
tAbstraction ::Parser (I TAbstraction)
tAbstraction = dec1 "Abstraction" $ do
  attrs <- tAttributes
  t <- optional $ try tAbstractionTail
  return TAbstraction {as = attrs, t = t}

data TAbstractionTail = TAbstractionTail {e::Options2 (I TSuffix, Maybe (Options2 (I TName) (I Terminal))) (I THtail)}
tAbstractionTail :: Parser (I TAbstractionTail)
tAbstractionTail = dec1 "Abstraction tail" $ do
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

data Load = Load {start::Position, end::Position}
initLoad = Load {start = Position 0 0, end = Position 0 0}

-- pAbstraction :: Parser Node
-- pAbstraction = dec "Abstraction" $ do
--   attrs <- {-debug "abstraction:attributes"-} pAttributes
--   t <-
--     optionalNode $
--       listNode $ choiceTry
--           [ do
--               suff <- {-debug "abstraction:suffix"-} pSuffix
--               o <-
--                 optionalNode $
--                   string cSPACE
--                     *> string cSLASH
--                     *> choiceTry
--                       [ {-debug "abstraction:name"-} pNAME,
--                         pTerminal cQUESTION QUESTION
--                       ]
--               return [suff, o],
--             do
--               htail <- {-debug "abstraction:htail"-} pHtail
--               return [htail]
--           ]
--   return (Abstraction, [attrs, t])

-- | contains list of arguments
--
-- If no arguments are provided, the list is empty
-- This is the same as making the part between [] optional

data TAttributes = TAttributes {as::[I TLabel]}
tAttributes :: Parser (I TAttributes)
tAttributes = dec1 "Attributes" $ do
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


-- pAttributes :: Parser Node
-- pAttributes = dec "Attributes" $ do
--   _ <- string cLSQ
--   attrs <- choiceTry
--         [ do
--             a <- {-debug "attributes:attribute1"-} pLabel
--             as <- manyTry (string cSPACE *> {-debug "attributes:attribute2"-} pLabel)
--             return (a : as),
--           [] <$ pEmpty
--         ]
--   _ <- string cRSQ
--   return (Attributes, attrs)

data TLabel = TLabel {l::Options2 (I Terminal) (I TName, Maybe (I Terminal))}
tLabel :: Parser (I TLabel)
tLabel = dec1 "Label" $ do
  l <-
    choiceTry
      [ Options2A <$ {-debug "label:@"-} (tTerminal cAT At),
        do
          name <- {-debug "label:name"-} pNAME
          -- TODO move dots to abstraction end (before csq)
          dots <- optional ({-debug "label:..."-} (tTerminal cDOTS Dots))
          return Options2B (name, dots)
      ]
  return TLabel {l = l}


-- pLabel :: Parser Node
-- pLabel = dec "Label" $ do
--   l <-
--     choiceTry
--       [ (: []) <$> {-debug "label:@"-} (pTerminal cAT AT),
--         do
--           name <- {-debug "label:name"-} pNAME
--           -- TODO move dots to abstraction end (before csq)
--           dots <- optionalNode ({-debug "label:..."-} (pTerminal cDOTS DOTS))
--           return [name, dots]
--       ]
--   return (Label, l)

data TTail = TTail {os::[I TObject]}
tTail :: Int -> Parser (I TTail)
tTail ind = dec1 "Tail" $ do
  let tObj = do
        e <- {-debug "tail:eol"-} tEOL_TAB_MANY
        let ind1 = getIndent e
        guard $ ind1 == ind
        tObject ind1
  objects <- someTry tObj
  return TTail {os = objects}

-- pTail :: Int -> Parser Node
-- pTail ind = dec "Tail" $ do
--   let pObj = do
--         e <- {-debug "tail:eol"-} pEOL_TAB_MANY
--         let ind1 = getIndent e
--         guard $ ind1 == ind
--         pObject ind1
--   objects <- someTry pObj
--   return (Tail, objects)

data TSuffix = TSuffix {l::I TLabel, c::Maybe (I Terminal)}
tSuffix :: Parser (I TSuffix)
tSuffix = dec1 "Suffix" $ do
  label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} tLabel
  c <- optional ({-debug "suffix:const"-} (tTerminal cCONST Const))
  return TSuffix {l = label, c = c}

-- pSuffix :: Parser Node
-- pSuffix = dec "Suffix" $ do
--   label <- string cSPACE *> string cARROW *> string cSPACE *> {-debug "suffix:label"-} pLabel
--   c <- optionalNode ({-debug "suffix:const"-} (pTerminal cCONST CONST))
--   return (Suffix, [label, c])

data TMethod = TMethod {m::Options2 (I TName) (I Terminal)}
tMethod :: Parser (I TMethod)
tMethod = dec1 "Method" $ do
  method <-
    string cDOT
      *> choiceTry
        [ Opt2A <$> {-debug "method:name"-} tNAME,
          Opt2B <$> {-debug "method:^"-} (tTerminal cRHO Rho),
          Opt2B <$> {-debug "method:@"-} (tTerminal cAT At),
          Opt2B <$> {-debug "method:<"-} (tTerminal cVERTEX Vertex)
        ]
  return TMethod {m = method}

-- pMethod :: Parser Node
-- pMethod = dec "Method" $ do
--   method <-
--     string cDOT
--       *> choiceTry
--         [ {-debug "method:name"-} pNAME,
--           {-debug "method:^"-} (pTerminal cRHO RHO),
--           {-debug "method:@"-} (pTerminal cAT AT),
--           {-debug "method:<"-} (pTerminal cVERTEX VERTEX)
--         ]
--   return (Method, [method])

data TApplication = TApplication {s::Options2 (I THead) (I TApplication), h::Maybe (I THtail), a1::I TApplication1}
tApplication :: Parser (I TApplication)
tApplication = dec1 "Application" $ do
  s <-
    choiceTry
      [ Opt2A <$>{-debug "application:head"-} tHead,
        Opt2B <$> (string cLB *> {-debug "application:application"-} tApplication <* string cRB)
      ]
  h <- optional $ try ({-debug "application:htail"-} tHtail)
  a1 <- {-debug "application:application1"-} tApplication1
  return TApplication {s = s, h = h, a1 = a1}

-- pApplication :: Parser Node
-- pApplication = dec "Application" $ do
--   s <-
--     choiceTry
--       [ {-debug "application:head"-} pHead,
--         string cLB *> {-debug "application:application"-} pApplication <* string cRB
--       ]
--   h <- optionalNode ({-debug "application:htail"-} pHtail)
--   a1 <- {-debug "application:application1"-} pApplication1
--   return (Application, [s, h, a1])

data TApplication1 = TApplication1 {c::Maybe (I TApplication1Elem)}
tApplication1 :: Parser (I TApplication1)
tApplication1 = dec1 "Application1" $ do
  c <- optional $ try tApplication1Elem
  return TApplication1 {c = c}

data TApplication1Elem = TApplication1Elem {c1::Options3 (I TMethod) (I THas) (I TSuffix), ht::Maybe (I THtail), a::I TApplication1}
tApplication1Elem :: Parser (I TApplication1Elem)
tApplication1Elem = dec1 "Application1 Element" $ do
  c1 <-
    choiceTry
      [ Opt3A <$> {-debug "application1:method"-} tMethod,
        Opt3B <$> {-debug "application1:has"-} tHas,
        Opt3C <$> {-debug "application1:suffix"-} tSuffix
      ]
  ht <- optional $ try ({-debug "application1:htail"-} tHtail)
  a <- {-debug "application1:application1"-} tApplication1
  return TApplication1Elem {c1 = c1, ht = ht, a = a}

-- pApplication1 :: Parser Node
-- pApplication1 = dec "Application1" $ do
--   c <- choiceTry
--         [ do
--             c1 <-
--               choiceTry
--                 [ {-debug "application1:method"-} pMethod,
--                   {-debug "application1:has"-} pHas,
--                   {-debug "application1:suffix"-} pSuffix
--                 ]
--             ht <- optionalNode ({-debug "application1:htail"-} pHtail)
--             a <- {-debug "application1:application1"-} pApplication1
--             return [c1, ht, a]
--           , [] <$ pEmpty
--         ]
--   return (Application1, c)

data THtail = THtail {t::[Options3 (I THead) (I TApplication) (I TAbstraction)]}
tHtail :: Parser (I THtail)
tHtail = dec1 "Htail" $ do
  let op = choiceTry
            [ Opt3A <$> {-debug "htail:head"-} tHead,
              Opt3B <$> (string cLB *> {-debug "htail:application1"-} tApplication <* string cRB),
              Opt3C <$> {-debug "htail:abstraction"-} tAbstraction
            ]
  t <- someTry (string cSPACE *> op)
  return THtail {t = t}

-- pHtail :: Parser Node
-- pHtail = dec "Htail" $ do
--   let op = choiceTry
--             [ {-debug "htail:head"-} pHead,
--               string cLB *> {-debug "htail:application1"-} pApplication <* string cRB,
--               {-debug "htail:abstraction"-} pAbstraction
--             ]
--   t <- someTry (string cSPACE *> op)
--   return (Htail, t)

data Options8 a b c d e f g h = 
    Options8A a
  | Options8B b
  | Options8C c
  | Options8D d
  | Options8E e
  | Options8F f
  | Options8G g
  | Options8H h

data Terminal =
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

tTerminal :: Text -> Terminal -> Parser (I Terminal)
tTerminal s t = dec1 s $ do
  p1 <- getPos
  void (string s)
  p2 <- getPos
  return t

data THead = THead {dots::Maybe (I Terminal), t::Options3 (I Terminal) (I THeadName) (I TData)}
tHead :: Parser (I THead)
tHead = dec1 "Head" $ do
  dots <- optional $ tTerminal cDOTS Dots
  t <- choiceTry
      [ Opt3A <$>{-debug "head:root"-}  (tTerminal cROOT Root),
        Opt3A <$>{-debug "head:at"-}  (tTerminal cAT At),
        Opt3A <$>{-debug "head:rho"-}  (tTerminal cRHO Rho),
        Opt3A <$>{-debug "head:xi"-}   (tTerminal cXI Xi),
        Opt3A <$>{-debug "head:sigma"-}   (tTerminal cSIGMA Sigma),
        Opt3A <$>{-debug "head:star"-}   (tTerminal cSTAR Star),
        Opt3B <$> tHeadName,
        Opt3C <$>{-debug "head:data"-} (tDATA)
      ]
  return THead {dots = dots, t = t}

-- TODO lookahead EOL
data THeadName = THeadName {name::I TName, c::Options2 (I Terminal) (Maybe (I Terminal))}
tHeadName :: Parser (I THeadName)
tHeadName = dec1 "Head name" $ do
  name <- tNAME
  c <- choiceTry [
        Opt2A <$> tTerminal cDOT Dot <* lookAhead (string cSPACE)
      , Opt2B <$> optional (tTerminal cCOPY Copy)
      ]
  return THeadName {name = name, c = c}

-- pHead :: Parser Node
-- pHead = dec "Head" $ do
--   dots <- optionalNode $ pTerminal cDOTS DOTS
--   t <- choiceTry
--       [ {-debug "head:root"-}  (pTerminal cROOT ROOT),
--         {-debug "head:at"-}  (pTerminal cAT AT),
--         {-debug "head:rho"-}  (pTerminal cRHO RHO),
--         {-debug "head:xi"-}   (pTerminal cXI XI),
--         {-debug "head:sigma"-}   (pTerminal cSIGMA SIGMA),
--         {-debug "head:star"-}   (pTerminal cSTAR STAR),
--         {-debug "head:copy"-} (
--           listNode $
--             do
--               name <- pNAME
--               c <- choiceTry [
--                     pTerminal cDOT DOT <* lookAhead (string cSPACE)
--                   , optionalNode $ pTerminal cCOPY COPY
--                   ]
--               return [name, c]),
--          ({-debug "head:data"-} pDATA)
--       ]
--   return (Head, [dots, t])

data THas = THas {n::I TName}
tHas :: Parser (I THas)
tHas = dec1 "Has" $ do
  _ <- string cCOLON
  n <- {-debug "has:name"-} tNAME
  return THas {n = n}

-- pHas :: Parser Node
-- pHas = dec "Has" $ do
--   _ <- string cCOLON
--   n <- {-debug "has:name"-} pNAME
--   return (Has, [n])

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

data TData = TData {d::Options9 (I TBool) (I TText) (I THex) (I TString) (I TFloat) (I TInt) (I TBytes) (I TChar) (I TRegex)}
tDATA :: Parser (I TData)
tDATA = dec1 "DATA" $ do
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

-- pDATA :: Parser Node
-- pDATA = dec "DATA" $ do
--   d <-
--     choiceTry
--       [ {-debug "data:bool"-} pBOOL,
--         {-debug "data:text"-} pTEXT,
--         {-debug "data:hex"-} pHEX,
--         {-debug "data:string"-} pSTRING,
--         {-debug "data:float"-} pFLOAT,
--         {-debug "data:int"-} pINT,
--         {-debug "data:bytes"-} pBYTES,
--         {-debug "data:char"-} pCHAR,
--         {-debug "data:regex"-} pREGEX
--       ]
--   return (Data, [d])

data TComment = TComment {c::Text}
tCOMMENT :: Parser (I TComment)
tCOMMENT = dec1 "COMMENT" $ do
  _ <- string cHASH
  content <- pack <$> many printChar
  return TComment {c = content}

-- pCOMMENT :: Parser Node
-- pCOMMENT = dec "COMMENT" $ do
--   _ <- string cHASH
--   content <- pack <$> many printChar
--   return (COMMENT content, [])

data TMeta = TMeta {name::I TName, suff::Maybe Text}
tMETA :: Parser (I TMeta)
tMETA = dec1 "META" $ do
  _ <- string cPLUS
  name <- {-debug "meta:name"-} tName
  suffix <- {-debug "meta:suffix"-} (optional . try $ pack <$> (string cSPACE *> some printChar))
  return TMeta {name = name, suff = suffix}

-- pMETA :: Parser Node
-- pMETA = dec "META" $ do
--   _ <- string cPLUS
--   name <- {-debug "meta:name"-} pNAME
--   suffix <- {-debug "meta:suffix"-} (optionalNode $ textNode $ pack <$> (string cSPACE *> some printChar))
--   return (META, [name, suffix])

data TRegex = TRegex {r :: Text, suff :: Text}
tREGEX :: Parser (I TRegex)
tREGEX = dec1 "REGEX" $ do
  _ <- string cSLASH
  r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
  _ <- string cSLASH
  suffix <- pack <$> many alphaNumChar
  return (TRegex r suffix)

-- pREGEX :: Parser Node
-- pREGEX = dec "REGEX" $ do
--   _ <- string cSLASH
--   r <- takeWhile1P (Just "regex expression") (`notElem` map T.head [cSLASH, cNEWLINE, cCARET_RETURN])
--   _ <- string cSLASH
--   suffix <- pack <$> many alphaNumChar
--   return (REGEX r suffix, [])

data TIndent = TIndent {n::Int}
tEOLTabMany :: Parser (I TIndent)
tEOLTabMany = dec1 "EOL_TAB_MANY" $ do
  _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
  indents <- T.concat <$> many (string cINDENT)
  let nIndents = T.length indents `div` 2
  return TIndent {n = nIndents}

-- pEOL_TAB_MANY :: Parser Node
-- pEOL_TAB_MANY = dec "EOL_TAB_MANY" $ do
--   _ <- {-debug "eol:eol"-} (try (eol *> optional eol))
--   indents <- T.concat <$> many (string cINDENT)
--   let nIndents = T.length indents `div` 2
--   return (INDENT nIndents, [])

data TByte = TByte {b :: Integer}
tBYTE :: Parser (I TByte)
tBYTE = dec1 "BYTE" $ do
  b <- hexToInt <$> count 2 pHexDigitUpper
  return TByte {b = b}

-- pBYTE :: Parser Node
-- pBYTE = dec "BYTE" $ do
--   b <- hexToInt <$> count 2 pHexDigitUpper
--   return (BYTE b, [])

data TLineBytes = TLineBytes {bs::[I TByte]}
tLineBytes :: Parser (I TLineBytes)
tLineBytes = dec1 "LINE_BYTES" $ do
  byte <- {-debug "line_bytes:byte"-} tBYTE
  bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> tBYTE))
  return TLineBytes {bs = byte : bytes}

-- pLINE_BYTES :: Parser Node
-- pLINE_BYTES = dec "LINE_BYTES" $ do
--   byte <- {-debug "line_bytes:byte"-} pBYTE
--   bytes <- {-debug "line_bytes:bytes"-} (someTry (string cMINUS *> pBYTE))
--   return (LINE_BYTES, byte : bytes)

data TBytes = TBytes {bs::Options3 (I Terminal) (I TByte) [I TLineBytes]}
tBYTES :: Parser (I TBytes)
tBYTES = dec1 "BYTES" $ do
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
      byte <- tBYTE
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

-- pBYTES :: Parser Node
-- pBYTES = dec "BYTES" $ do
--   bytes <-
--     choiceTry
--       [ parser1,
--         parser3,
--         parser2
--       ]
--   return (BYTES, bytes)
--   where
--     parser1 = do
--       _ <- string cEMPTY_BYTES
--       return []
--     parser2 = do
--       byte <- pBYTE
--       _ <- string cMINUS
--       return [byte]
--     parser4 = do
--       _ <- string cMINUS
--       -- TODO guard indentation
--       e <- pEOL_TAB_MANY
--       lb <- pLINE_BYTES
--       return [e, lb]
--     parser3 = do
--       lb <- pLINE_BYTES
--       lbs <- concat <$> manyTry parser4
--       return (lb : lbs)

data TBool = TBool {b::Bool}
tBool :: Parser (I TBool)
tBool = dec1 "BOOL" $ do
  b <-
    choiceTry
      [ True <$ string cTRUE,
        False <$ string cFALSE
      ]
  return TBool {b = b} 

-- pBOOL :: Parser Node
-- pBOOL = dec "BOOL" $ do
--   b <-
--     choiceTry
--       [ BOOL True <$ string cTRUE,
--         BOOL False <$ string cFALSE
--       ]
--   return (b, [])

data TChar = TChar {c::Char}
-- | slightly differs from grammar: doesn't allow u Byte Byte
tChar :: Parser (I TChar)
tChar = dec1 "CHAR" $ do
  c <- char '\'' *> charLiteral <* char '\''
  return TChar {c = c}

-- | slightly differs from grammar: doesn't allow u Byte Byte
-- pCHAR :: Parser Node
-- pCHAR = dec "CHAR" $ do
--   c <- char '\'' *> charLiteral <* char '\''
--   return (CHAR c, [])

data TString = TString {s::Text}
-- | slightly differs from grammar: doesn't allow u Byte Byte
tString :: Parser (I TString)
tString = dec1 "STRING" $ do
  s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
  return TString {s = s}

-- | slightly differs from grammar: doesn't allow u Byte Byte
-- pSTRING :: Parser Node
-- pSTRING = dec "STRING" $ do
--   s <- pack <$> (char '\"' *> manyTill charLiteral (char '\"'))
--   return (STRING s, [])

data TInt = TInt {s::Integer}
tInt :: Parser (I TInt)
tInt = dec1 "INT" $ do
  s <- signed pEmpty decimal
  return TInt {s = s}

-- pINT :: Parser Node
-- pINT = dec "INT" $ do
--   s <- signed pEmpty decimal
--   return (INT s, [])

data TFloat = TFloat {f::Scientific}
tFloat :: Parser (I TFloat)
tFloat = dec1 "FLOAT" $ do
  f <- signed pEmpty scientific
  return (TFloat {f = f})

-- pFLOAT :: Parser Node
-- pFLOAT = dec "FLOAT" $ do
--   f <- signed pEmpty scientific
--   return (FLOAT f, [])

data THex = THex {h::Integer}
tHex :: Parser (I THex)
tHex = dec1 "HEX" $ do
  s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
  return THex {h = s}


-- pHEX :: Parser Node
-- pHEX = dec "HEX" $ do
--   s <- hexToInt <$> (string "0x" *> someTry pHexDigitLower)
--   return (HEX s, [])

data TName = TName {n::Text}
tName :: Parser (I TName)
tName = dec1 "NAME" $ do
  l1 <- {-debug "name: first letter"-} lowerChar
  l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
  return TName {n = pack (l1:l2)}

-- pNAME :: Parser Node
-- pNAME = dec "NAME" $ do
--   l1 <- {-debug "name: first letter"-} lowerChar
--   l2 <- {-debug "name: other letters"-} (manyTry (letterChar <|> numberChar <|> char '_' <|> char '-'))
--   return (NAME (pack (l1 : l2)), [])



--     LText TText

newtype TText = TText {t::Text}

tText :: Parser (I TText)
tText = dec1 "TEXT" $ do
  t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
  return TText {t = t}

-- IDK maybe need to allow indentation after eol
-- pTEXT :: Parser Node
-- pTEXT = dec "TEXT" $ do
--   t <- try $ pack <$> (string cTEXT_MARK *> eol *> manyTill charLiteral (string cTEXT_MARK))
--   return (TEXT t, [])


-- TODO function to combine loads