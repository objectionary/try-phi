{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintTerm where
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
  Abstraction (..),
  AbstractionTail(..),
  Label(..),
  MethodName(..),
  Head(..),
  HeadName(..),
  LetterName(..),
  Modifier(..),
  SuffixName(..)
  )

import ParseEO(cRHO, cVERTEX, Options3(..), cROOT, cSIGMA, cSTAR, cXI, cCOPY, cCONST, cQUESTION)

import Data.Char (toUpper)
import Data.Text (unpack)
import Text.Printf (printf)
import ParseEO (Options2(..), cAT, cDOTS, THeadTerminal(..), cDOT, TAbstrQuestion)
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
      DBool a -> show a
      DChar a -> toUpper <$> show a
      DFloat a -> show a
      DHex a -> showHex a ""
      DInt a -> show a
      DString a -> unpack a
      DText a -> unpack a
      DRegex a c -> printf "/%s/%s" (pprint' a) (pprint' c)
      DBytes (Opt2A a) -> pprint' a
      DBytes (Opt2B a) -> intercalate "-" $ pprint' <$> a

instance PPTermInline (K HasName) where
  pprint' Ann {term = t1} = unpack $
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
    printf "%s%s"
    (pprint' n)
    (unpack $ if isConst then cCONST else "")

instance PPTermInline (K TAbstrQuestion) where
  pprint' _ = unpack cQUESTION

instance PPTermInline AttachedName  where
  pprint' AttachedName {..} =
    printf " %s" (pprint' a) <>
    maybe "" f imported
    where
      f e = printf " %s" $
        case e of
          Opt2A b -> pprint' b
          Opt2B b -> pprint' b

tab :: [Char]
tab = "  "
tabs :: Int -> [Char]
tabs m = concat $ replicate m tab

instance PPTermIndented [AttachedOrArg] where
  pprint m ls = intercalate ("\n" <> tabs m) (pprint m <$> ls)

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
  pprint' Ann {term = Head {..}} =
    case h of
     Opt3A ann -> pprint' ann
     Opt3B ann -> pprint' ann
     Opt3C ann -> pprint' ann


-- TODO
-- some terms should be printed inline
-- like application's term
-- otherwise, we'll have to pass suffixname inside it

instance PPTermIndented (K Term) where
  pprint m Ann {term = t1} = case t1 of
    -- expr on first line
    -- args and bindings on the next lines
    App s t -> pprint m s <> intercalate "\n" (pprint (m + 1) <$> t)
    Obj s t  -> pprint' s <> intercalate "\n" (pprint (m + 1) <$> t)
    Dot s t -> pprint m s <> pprint' t
    HeadTerm s t ->
      case (s, t) of
        (Just a, Just b) -> printf "%s.%s" (show a) (pprint' b)
        (Just a, Nothing) -> printf "%s" (show a)
        (Nothing, Just b) -> printf "%s" (pprint' b)
        _ -> ""

instance PPTermIndented AttachedOrArg where
  pprint m AttachedOrArg {t = Ann {term = t1}, a = a1} = case t1 of
    App ann aoas ->
      printf "%s%s\n%s"
        (pprint m ann)
        (case a1 of
            Opt2A a -> pprint' a
            Opt2B a -> maybe "" pprint' a
        )
        (unwords (printf "(%s)" . pprint m <$> aoas))
    -- TODO
    Obj f as ->
      printf "%s"
    Dot ann ann' -> ""
    HeadTerm m_n m_ann -> ""

{-
when printing app or obj
print term of attributes, then suffix,
then args as a tail
-}

