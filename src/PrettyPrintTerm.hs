{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module PrettyPrintTerm (pprintTermProgram) where
import Numeric (showHex)

import ToTerm(
  Term(..),
  DataValue(..),
  Ann(..),
  DByte(..),
  DRegexBody(..),
  DRegexSuffix(..),
  DLineBytes(..),
  HasName (..),
  AttachedName (..),
  AttachedOrArgument (..),
  Label(..),
  MethodName(..),
  Head(..),
  HeadName(..),
  LetterName(..),
  Modifier(..),
  SuffixName(..),
  AttachedOrArgName,
  HeadTerminal(..),
  AbstrQuestion
  )

import ParseEO
    ( cRHO,
      cVERTEX,
      Options3(..),
      cROOT,
      cSIGMA,
      cSTAR,
      cXI,
      cCOPY,
      cCONST,
      cQUESTION,
      Options2(..),
      cAT,
      cDOTS,
      cDOT,
      TAbstrQuestion )

import Data.Char (toUpper)
import Data.Text (unpack)
import Text.Printf (printf)
import Data.List(intercalate)


class PPTermIndented a where
  -- | print with given indentation level 
  pprint :: Int -> a -> String

class PPTermInline a where
  -- | print inline
  pprint' :: a -> String

instance PPTermInline (DByte) where
  pprint' DByte {..} = toUpper <$> showHex byte ""

instance PPTermInline (DRegexBody) where
  pprint' DRegexBody {..} = unpack b

instance PPTermInline (DRegexSuffix) where
  pprint' DRegexSuffix {..} = unpack s

instance PPTermInline DLineBytes where
  pprint' DLineBytes {bs} = intercalate "-" (pprint' <$>  bs)

instance PPTermInline DataValue where
  pprint' pt =
    case pt of
      DBool {..}  -> toUpper <$> show b
      DChar {..}  -> "\'" <> show c <> "\'"
      DFloat {..}  -> show f
      DHex {..}  -> showHex h ""
      DInt {..}  -> show i
      DString {..}  -> "\"" <> unpack s <> "\""
      DText {..}  -> "\"\"\"" <> unpack t <> "\"\"\""
      DRegex {..}  -> printf "/%s/%s" (pprint' rb) (pprint' rs)
      DBytes {bs = (Opt2A a)} -> pprint' a
      DBytes {bs = (Opt2B a)} -> intercalate "-" $ pprint' <$> a

instance PPTermInline HasName where
  pprint' pt = printf ":%s" $ unpack $
    case pt of
      HName {t} -> t
      HAt {} -> cAT

instance PPTermInline Label where
  pprint' t1 = unpack $
    case t1 of
      LName {n} -> n
      LAt {} -> cAT
      LVarArg {t} -> t <> cDOTS

instance PPTermInline [Label] where
  pprint' ls = printf "[%s]" (unwords (pprint' <$> ls))

instance PPTermInline SuffixName where
  pprint' SuffixName {..} =
    printf " > %s%s"
    (pprint' n)
    (unpack $ if isConst then cCONST else "")

instance PPTermInline AbstrQuestion where
  pprint' _ = unpack cQUESTION

instance PPTermInline AttachedName  where
  pprint' AttachedName {..} =
    printf "%s%s" (pprint' a) (maybe "" f imported)
    where
      f :: Options2 LetterName AbstrQuestion -> String
      f e = printf " %s" $
        case e of
          Opt2A b -> pprint' b
          Opt2B b -> pprint' b

tab :: [Char]
tab = "  "
tabs :: Int -> [Char]
tabs m = concat $ replicate m tab

instance PPTermIndented [AttachedOrArgument] where
  pprint m ls = printTailIndented m ls

instance PPTermInline [AttachedOrArgument] where
  pprint' ls = printTailInline ls

instance PPTermInline (MethodName) where
  pprint' t1 = unpack $
    case t1 of
      MName {n} -> n
      MRho {} -> cRHO
      MAt {} -> cAT
      MVertex {} -> cVERTEX

instance PPTermInline HeadTerminal where
  pprint' t1 = unpack $
    case t1 of
      HeadRoot {} -> cROOT
      HeadAt {} -> cAT
      HeadRho {} -> cRHO
      HeadXi {} -> cXI
      HeadSigma {} -> cSIGMA
      HeadStar {} -> cSTAR

instance PPTermInline LetterName where
  pprint' LetterName {n} = unpack n

instance PPTermInline Modifier where
  pprint' t1 = unpack $
    case t1 of
      MCopy {}-> cCOPY
      MInverseDot {} -> cDOT

instance PPTermInline HeadName where
  pprint' HeadName {..} = pprint' n <> maybe "" pprint' m

instance PPTermInline Head where
  pprint' Head {..} = u' <> h'
    where
      h' =
        case h of
          Opt3A ann -> pprint' ann
          Opt3B ann -> pprint' ann
          Opt3C ann -> pprint' ann
      u'
        | unpacked = unpack cDOTS
        | otherwise = ""

-- TODO print datavalue indented

printHead :: Maybe Int -> Maybe Head -> String
printHead s t =
  case (s, t) of
    (Just a, Just b) -> printf "%s.%s" (show a) (pprint' b)
    (Just a, Nothing) -> printf (unpack cRHO  <> "%s") (show a)
    (Nothing, Just b) -> printf "%s" (pprint' b)
    _ -> ""


-- TODO print dot before methodname

instance PPTermInline AttachedOrArgName where
  pprint' (Opt2A a) = pprint' a
  pprint' (Opt2B a) = maybe "" pprint' a


parens :: String -> String
parens x = "(" <> x <> ")"


printTailInline :: PPTermInline a => [a] -> [Char]
printTailInline bs = if null bs then "" else " " <> unwords (lessParens . pprint' <$> bs)


printTailIndented :: Int -> [AttachedOrArgument] -> [Char]
printTailIndented m bs = intercalate "" (map (("\n" <> tabs m) <>) (pprint m <$> bs))

instance PPTermInline [MethodName] where
  pprint' xs = concatMap ("." <>) (pprint' <$> xs)


lessParens :: String -> String
lessParens s =
  case words s of
    [t] -> t
    t -> parens $ unwords t

-- TODO improve performance
withLessParens' :: (PPTermInline a, PPTermInline x) => x -> a -> String -> String
withLessParens' x a y = printf "%s" (lessParens (pprint' x <> y <> pprint' a))

pprintTermNamed' :: PPTermInline a => Term -> a -> String
pprintTermNamed' pt pa =
  case pt of
      App {..} -> withLessParens' t pa (printTailInline args)
      Obj {..} -> withLessParens' freeAttrs pa (printTailInline attrs)
      Dot {..} -> parens $ pprint' t <> pprint' attr <> pprint' pa
      HeadTerm {..}  -> printf "%s%s" (printHead n a) (pprint' pa)


instance PPTermInline AttachedOrArgument where
  pprint' AttachedOrArgument {..} = pprintTermNamed' t a

instance PPTermInline String where
  pprint' = id

instance PPTermInline Term where
  pprint' t1 = pprintTermNamed' t1 (""::String)

instance PPTermInline [AttachedOrArgName] where
  pprint' xs = intercalate "" (pprint' <$> xs)

instance PPTermIndented [MethodName] where
  -- a. b.
  pprint m xs = unwords ((<> ".") <$> (pprint' <$> xs))

isInlineContiguous :: String -> Bool
isInlineContiguous s =
  case words s of
    [_] -> True
    _ -> False


{-| takes indentation level, term, name of this term and produces a string

-- TODO don't parenthesize top level expressions
-- (a) > t
--   b
-}
pprintTermNamed :: PPTermInline a => Int -> Term -> a -> String
pprintTermNamed m pt pa =
  case pt of
      App {..} -> printf "%s%s%s" (pprint' t) (pprint' args) (printTailIndented (m + 1) args)
      Obj {..} -> printf "%s%s%s" (pprint' freeAttrs) (pprint' attrs) (printTailIndented (m + 1) attrs)
      Dot {..}
        | isInlineContiguous (pprint' t) -> pprintTermNamed' pt pa
        | otherwise -> printf "%s%s\n%s%s" (pprint m attr) (pprint' pa) (tabs (m + 1)) (pprint (m + 1) t)
      HeadTerm {..}  -> printf "%s%s" (printHead n a) (pprint' pa)

instance PPTermIndented Term where
  pprint m t1 = pprintTermNamed m t1 (""::String)

instance PPTermIndented AttachedOrArgument where
  pprint m AttachedOrArgument {..} = pprintTermNamed m t a

{- |
print annotated top term - the whole program
-}
pprintTermProgram :: Term -> String
pprintTermProgram t =
    case t of
      Obj {attrs} -> intercalate "\n\n" (pprint 0 <$> attrs)
      _ -> ""
{-
when printing app or obj
print term of attributes, then suffix,
then args as a tail
-}