{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Phi.Minimal.Parser where

import           Control.Applicative       ((<|>))
import           Data.Char                 (isPrint, isSpace)
import           Data.Function             ((&))
import qualified Data.HashSet              as HashSet
import           Data.Text.Prettyprint.Doc as Doc
import           GHC.Exts                  (fromList)
import           Text.Parser.Token         ()
import           Text.Parser.Token.Style   (emptyIdents)
import           Text.Trifecta             (IdentifierStyle (..), Parser,
                                            TokenParsing, symbol)
import           Text.Trifecta             ((<?>))
import qualified Text.Trifecta             as Trifecta

import           Phi.Minimal.Model

parseTerm :: String -> Either String Term
parseTerm = parseString (pTerm <* Trifecta.eof)

pTerm :: (TokenParsing m, Monad m) => m Term
pTerm = foldl (&) <$> pNotDotAppTerm <*> Trifecta.many pDotApp

pNotDotAppTerm :: (TokenParsing m, Monad m) => m Term
pNotDotAppTerm = pLoc <|> pObj

pDotApp :: (TokenParsing m, Monad m) => m (Term -> Term)
pDotApp = pDot <|> pApp

pDot :: (TokenParsing m, Monad m) => m (Term -> Term)
pDot = flip Dot <$ symbol "." <*> pAttr

pApp :: (TokenParsing m, Monad m) => m (Term -> Term)
pApp = flip App <$> Trifecta.parens
  ((,) <$> pAttr <* (symbol "->" <|> symbol "↦") <*> pTerm)

pLoc :: (TokenParsing m, Monad m) => m Term
pLoc = Loc . fromIntegral
  <$  ((symbol "^" <|> symbol "ρ") <?> "locator")
  <*> (Trifecta.integer <|> superscriptInteger)

superscriptInteger :: Trifecta.TokenParsing m => m Integer
superscriptInteger = Trifecta.token $ read . map toDigit <$>
  Trifecta.some (Trifecta.oneOf "¹²³⁴⁵⁶⁷⁸⁹⁰")
  where
    toDigit c =
      case lookup c (zip "¹²³⁴⁵⁶⁷⁸⁹⁰" "1234567890") of
        Just c' -> c'
        Nothing -> error ("invalid superscript digit: " <> show c)

pObj :: (TokenParsing m, Monad m) => m Term
pObj = Obj . fromList <$> Trifecta.between
  (symbol "[" <|> symbol "⟦")
  (symbol "]" <|> symbol "⟧")
  (pAttrWithValue `Trifecta.sepBy` Trifecta.comma)

pAttrWithValue :: (TokenParsing m, Monad m) => m (Attr, AttrValue Term)
pAttrWithValue = (,) <$> pAttr <* (symbol "->" <|> symbol "↦") <*> pAttrValue

pAttrValue :: (TokenParsing m, Monad m) => m (AttrValue Term)
pAttrValue = pVoidAttr <|> pAttachedAttr
  where
    pVoidAttr = VoidAttr <$ (symbol "?" <|> symbol "ø")
    pAttachedAttr = Attached <$> pTerm

pAttr :: (TokenParsing m, Monad m) => m Attr
pAttr = normalise <$> pIdent
  where
    normalise = \case
      "@" -> "𝜑"
      a -> a

pIdent :: (TokenParsing m, Monad m) => m String
pIdent = Trifecta.ident pIdentStyle

pIdentStyle :: (TokenParsing m, Monad m) => IdentifierStyle m
pIdentStyle = (emptyIdents @Parser)
  { _styleStart     = Trifecta.satisfy isIdentChar
  , _styleLetter    = Trifecta.satisfy isIdentChar
  , _styleReserved  = HashSet.fromList [ ]
  }

-- ** Char predicates

isIdentChar :: Char -> Bool
isIdentChar c = isPrint c && not (isSpace c) && not (isDelim c)

isDelim :: Char -> Bool
isDelim c = c `elem` ("()[]{},⟦⟧↦." :: String)

-- ** Helpers

(<??>) :: String -> Parser a -> Parser a
(<??>) = flip (<?>)

parseString :: Parser a -> String -> Either String a
parseString parser input =
  case Trifecta.parseString parser mempty input of
    Trifecta.Success x       -> Right x
    Trifecta.Failure errInfo -> Left (show (Doc.unAnnotate (Trifecta._errDoc errInfo)))
