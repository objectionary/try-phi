{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Text                  as T (Text, unpack)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec            (runParser)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

main :: IO ()
main = do
    text <- TIO.readFile "app/code.eo"
    let str = T.unpack text
    parseTest pUri str


-- newtype Comment = Comment { line :: String }

-- data Meta =
--   Meta {
--     keyword :: String
--   , path    :: String
--   } deriving (Eq, Show)

-- data EOprogram =
--   EOprogram {
--     license :: [Comment]
--   , metas   :: [Meta]
--   -- , objects :: [Object]
--   }

-- type AttrName = String

-- data AttrValue = Term | [Object]

-- data Object =
--   Object {
--     declaration :: Declaration
--   , definition :: Map AttrName String
--   }

-- Symbols
arrayStar :: [Char]
arrayStar = "*"

const :: [Char]
const = "!"

colon :: [Char]
colon = ":"

copy :: [Char]
copy = "\'"

vertex :: [Char]
vertex = "<"

root :: Char
root = 'Q'

sigma :: [Char]
sigma = "&"

xi :: [Char]
xi = "$"

dots :: [Char]
dots = "..."

slash :: [Char]
slash = "/"

arrow :: [Char]
arrow = ">"

plus :: [Char]
plus = "+"

minus :: [Char]
minus = "-"

dot :: [Char]
dot = "."

at :: [Char]
at = "@"

rho :: [Char]
rho = "^"

hash :: [Char]
hash = "#"

eol :: [Char]
eol = "\n"

space :: [Char]
space = " "

lsq :: [Char]
lsq = "["

rsq :: [Char]
rsq = "]"

lb :: [Char]
lb = "("

rb :: [Char]
rb = ")"

-- other tokens
-- no conversion to values happens in an AST
-- i.e., we keep values as strings

-- TODO group token constructors to allow 
-- e.g. License be constructed only from COMMENTs

data TokenValue =
    COMMENT String
  | META {keyword::String, path::String}
  | REGEX String
  | BYTES String
  | INT String
  | FLOAT String
  | BOOL String
  | CHAR String
  | HEX String
  | STRING String
  | FREE_NAME String
  | FOREIGN_NAME String
  | TEXT String

data TOKEN =
  TOKEN {
    value :: TokenValue
  , position :: Position
  }

data LICENSE = 
  LICENSE {
    license :: [TOKEN]
  , position :: Position
  }

data METAS = 
  METAS {
    metas :: [TOKEN]
  , position :: Position
  }
data PROGRAM = 
  PROGRAM {
    license :: Maybe LICENSE
  , metas :: Maybe METAS
  , newline :: Maybe String
  , objects :: [OBJECT]
  , position :: Position
  }

data OBJECT =
  OBJECT {
    declaration :: DECLARATION
  , spaces :: Int
  , definitions :: [DEFINITION]
  , position :: Position
  }

data DeclarationValue =
    DeclarationNamed {attrName :: TOKEN, freeAttrs :: [TOKEN]}
  | DeclarationAnonymous {freeAttrs :: [TOKEN]}

data DECLARATION =
  DECLARATION {
    value :: DeclarationValue
  , position :: Position
  }

data DefinitionValue =
    DefinitionObject OBJECT
  | DefinitionInlineExpr INLINE

data DEFINITION = 
  DEFINITION {
    value :: DefinitionValue
  , position :: Position
  }
  
data INLINE = 
  INLINE {
    tokens :: [INLINE_TOKEN]
  , position :: Position
  }

data InlineValue =
    InlineParenthesized PARENTHESIZED
  | InlineData TOKEN
  | InlineAction ACTION

data Position =
  Position {
    from :: Int
  , to :: Int
  }

data INLINE_TOKEN = 
  INLINE_TOKEN {
    argument :: Maybe ARGUMENT
  , value :: InlineValue
  , position :: Position
  }

data ParenthesizedValue = 
    ParenthesizedInline INLINE
  | ParenthesizedAnonymous ANONYMOUS

data PARENTHESIZED =
  PARENTHESIZED {
    value :: ParenthesizedValue
  , position :: Position
  }

data ActionValue = 
    ActionDotAccess DOT_ACCESS
  | ActionInverseDot INVERSE_DOT
  | ActionApplication APPLICATION

data ACTION =
  ACTION {
    value :: ActionValue
  , position :: Position
  }

data ArgumentValue = ArgumentFreeName TOKEN

data ARGUMENT = 
  ARGUMENT {
    value :: ArgumentValue
  , position :: Position
  }

data AnonymousValue = 
    AnonymousDeclaration TOKEN [INLINE_ATTRIBUTE]
  | AnonymousArray Maybe INLINE

data ANONYMOUS =
  ANONYMOUS {
    value :: AnonymousValue
  , position :: Position
  }

data DOT_ACCESS = 
  DOT_ACCESS {
    value :: 
  }






pUri :: Parser Uri
pUri = do
  r <- pScheme
  _ <- char ':'
  return (Uri r)

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  } deriving (Eq, Show)

-- store strings in AST?

-- pMeta :: Parser Meta
-- pMeta =

-- 2 screens EO and dialect
