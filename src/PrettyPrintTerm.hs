{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module PrettyPrintTerm (pprint, pprintTop) where
import Numeric (showHex)

import ToTerm(
  Term(..),
  K,
  DataValue(..),
  Ann(..),
  DByte(..),
  DRegexBody(..),
  DRegexSuffix(..),
  DLineBytes(..),
  HasName (..),
  AttachedName (..),
  AttachedOrArg (..),
  Label(..),
  MethodName(..),
  Head(..),
  HeadName(..),
  LetterName(..),
  Modifier(..),
  SuffixName(..),
  AttachedOrArgName
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
      THeadTerminal(..),
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
  pprint' :: a -> String

instance PPTermInline (K DByte) where
  pprint' Ann {term = DByte t1} = toUpper <$> showHex t1 ""

instance PPTermInline (K DRegexBody) where
  pprint' Ann {term = DRegexBody t1} = unpack t1

instance PPTermInline (K DRegexSuffix) where
  pprint' Ann {term = DRegexSuffix t1} = unpack t1

instance PPTermInline (K DLineBytes) where
  pprint' ls = intercalate "-" (f ls)
    where
      f :: K DLineBytes -> [String]
      f Ann {term = DLineBytes {bs = t1}} = pprint' <$> t1

instance PPTermInline (K DataValue) where
  pprint' Ann {term = t1} =
    case t1 of
      DBool a -> toUpper <$> show a
      DChar a -> "\'" <> show a <> "\'"
      DFloat a -> show a
      DHex a -> showHex a ""
      DInt a -> show a
      DString a -> "\"" <> unpack a <> "\""
      DText a -> "\"\"\"" <> unpack a <> "\"\"\""
      DRegex a c -> printf "/%s/%s" (pprint' a) (pprint' c)
      DBytes (Opt2A a) -> pprint' a
      DBytes (Opt2B a) -> intercalate "-" $ pprint' <$> a

instance PPTermInline (K HasName) where
  pprint' Ann {term = t1} = printf ":%s" $ unpack $
    case t1 of
      HName txt -> txt
      HAt -> cAT

instance PPTermInline (K Label) where
  pprint' Ann {term = t1} = unpack $
    case t1 of
      LName txt -> txt
      LAt -> cAT
      LVarArg txt -> txt <> cDOTS

instance PPTermInline [K Label] where
  pprint' ls = printf "[%s]" (unwords (pprint' <$> ls))

instance PPTermInline SuffixName where
  pprint' SuffixName {..} =
    printf " > %s%s"
    (pprint' n)
    (unpack $ if isConst then cCONST else "")

instance PPTermInline (K TAbstrQuestion) where
  pprint' _ = unpack cQUESTION

instance PPTermInline AttachedName  where
  pprint' AttachedName {..} =
    printf "%s%s" (pprint' a) (maybe "" f imported)
    where
      f :: Options2 (K LetterName) (K TAbstrQuestion) -> String
      f e = printf " %s" $
        case e of
          Opt2A b -> pprint' b
          Opt2B b -> pprint' b

tab :: [Char]
tab = "  "
tabs :: Int -> [Char]
tabs m = concat $ replicate m tab

instance PPTermIndented [AttachedOrArg] where
  pprint m ls = printTailIndented m ls

instance PPTermInline [AttachedOrArg] where
  pprint' ls = printTailInline ls

instance PPTermInline (K MethodName) where
  pprint' Ann {term = t1} =  unpack $
    case t1 of
      MName txt -> txt
      MRho -> cRHO
      MAt -> cAT
      MVertex -> cVERTEX

instance PPTermInline (K THeadTerminal) where
  pprint' Ann {term = t1} = unpack $
    case t1 of
      HeadRoot -> cROOT
      HeadAt -> cAT
      HeadRho -> cRHO
      HeadXi -> cXI
      HeadSigma -> cSIGMA
      HeadStar -> cSTAR

instance PPTermInline (K LetterName) where
  pprint' Ann {term = LetterName t} = unpack t

instance PPTermInline Modifier where
  pprint' t1 = unpack $
    case t1 of
      MCopy -> cCOPY
      MInverseDot -> cDOT

instance PPTermInline (K HeadName) where
  pprint' Ann {term = HeadName {..}} = pprint' n <> maybe "" pprint' m

instance PPTermInline (K Head) where
  pprint' Ann {term = Head {..}} = u' <> h'
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

printHead :: Maybe Int -> Maybe (K Head) -> String
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
printTailInline bs = if null bs then "" else " " <> unwords (parens . pprint' <$> bs)


printTailIndented :: PPTermIndented a => Int -> [a] -> [Char]
printTailIndented m bs = intercalate "" (zipWith (<>) (repeat ("\n" <> tabs m)) (pprint m <$> bs))


pprintTermNamed' :: PPTermInline a => Term -> a -> String
pprintTermNamed' t a = 
  case t of
      App x y -> printf "%s%s%s" (pprint' x) (pprint' a) (printTailInline y)
      Obj x y -> printf "(%s%s)%s" (pprint' x) (pprint' a) (printTailInline y)
      Dot x y -> printf "(%s).%s%s" (pprint' x) (pprint' a) (pprint' y)
      HeadTerm x y  -> printHead x y


instance PPTermInline AttachedOrArg where
  pprint' AttachedOrArg {t = Ann {term = t1}, a = a1} =
    pprintTermNamed' t1 a1
    
instance PPTermInline String where
  pprint' = id

instance PPTermInline (K Term) where
  pprint' Ann {term = t1} = pprintTermNamed' t1 (""::String)

instance PPTermInline [AttachedOrArgName] where
  pprint' ls = intercalate "" (pprint' <$> ls)

pprintTermNamed :: PPTermInline a => Int -> Term -> a -> String
pprintTermNamed m t a = 
  case t of
      App x y -> printf "%s%s%s" (pprint' x) (pprint' a) (printTailIndented (m + 1) y)
      Obj x y -> printf "%s%s%s" (pprint' x) (pprint' a) (printTailIndented (m + 1) y)
      Dot x y -> printf "%s.%s\n%s%s" (pprint' y) (pprint' a) (tabs m) (pprint (m + 1) x)
      HeadTerm x y  -> printHead x y

instance PPTermIndented (K Term) where
  pprint m Ann {term = t1} = pprintTermNamed m t1 (""::String)

instance PPTermIndented AttachedOrArg where
  pprint m AttachedOrArg {t = Ann {term = t1}, a = a1} = pprintTermNamed m t1 a1

pprintTop :: K Term -> String
pprintTop Ann {term = t} =
    case t of
      Obj _ b -> intercalate "\n\n" (pprint 0 <$> b)
      _ -> ""
{-
when printing app or obj
print term of attributes, then suffix,
then args as a tail
-}

