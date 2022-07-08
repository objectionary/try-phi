{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances #-}
module PrettyPrintTree (pprintTree, ShowIndented) where

import ParseEO

import qualified Data.List as DL(intercalate)
import           Text.Printf                (printf, PrintfType)
import Data.Data (Data(toConstr))


-- printing utilities

-- wrapper for printing nodes as indented

newtype Indented a = Indented a

instance ShowIndented a => Show (Indented a) where
  show (Indented x) = showIndented 0 x

pprintTree :: ShowIndented a => a -> String
pprintTree t = show (Indented t)

-- Print helpers

tab :: String
tab = "|  "

tabs :: Int -> String
tabs n = DL.intercalate "" (replicate n tab)

constructorName :: Data a => a -> String
constructorName n = show $ toConstr n

showHead :: (PrintfType t, Data a, EpiAnn a) => Int -> a -> t
showHead n m = printf "%s%s %s %s" (tabs n) (tail $ constructorName m) (show segment) (show num)
  where
    Ann {segment = segment, num = num} = get m

nothing :: String
nothing = "Nothing"

printNonLeaf :: (Data a, Foldable t, Functor t, EpiAnn a) =>
  Int -> a -> t (Int -> String) -> String
printNonLeaf n i l = showHead n i <> "\n" <> printList ($ n + 1) l

printLeaf :: (Show t, Data a, EpiAnn a) =>
  Int -> a -> t -> String
printLeaf n i d = showHead n i <>  " " <> show d <> "\n"

printMaybe :: (Int -> a -> String) -> Int -> Maybe a -> String
printMaybe f k = maybe (printNothing k) (f k)

printList :: (Foldable t, Functor t) => (a1 -> [a2]) -> t a1 -> [a2]
printList f t = concat (f <$> t)

-- Node printers

instance ShowIndented a => ShowIndented (Maybe a) where
  showIndented m (Just a) = showIndented m a
  showIndented m Nothing = printNothing m

class ShowIndented a where
  showIndented :: Int -> a -> String

instance ShowIndented TProgram where
  showIndented n pt@TProgram {..} = printNonLeaf n pt [l', m', o']
    where
      l' k = showIndented k l
      m' k = showIndented k m
      o' k = showIndented k o

instance ShowIndented TLicense where
  showIndented n pt@TLicense {..} = printNonLeaf n pt [cs']
    where
      cs' k = concat (showIndented k <$> cs)

instance ShowIndented TComment where
  showIndented n pt@TComment {..} = printLeaf n pt c

instance ShowIndented TMetas where
  showIndented n pt@TMetas {..} = printNonLeaf n pt [cs']
    where
      cs' k = concat (showIndented k <$> ms)

printNothing :: Int -> String
printNothing n = tabs n <> nothing <> "\n"

instance ShowIndented TMeta where
  showIndented n pt@TMeta {..} = printNonLeaf n pt [name', suff']
    where
      name' k = showIndented k name
      suff' k = showIndented k suff

instance ShowIndented TMetaSuffix where
  showIndented n pt@TMetaSuffix {..} = printLeaf n pt s

instance ShowIndented TName where
  showIndented m pt@TName {..} = printLeaf m pt n

instance ShowIndented TVarArg where
  showIndented m pt@TVarArg {..} = printLeaf m pt n

instance ShowIndented TObjects where
  showIndented n pt@TObjects {..} = printNonLeaf n pt [os']
    where
      os' k = concat (showIndented k <$> os)

instance ShowIndented TObjectTail where
  showIndented n pt@TObjectTail {..} = printNonLeaf n pt [m', h', s', t']
    where
      m' k = showIndented k m
      h' k = showIndented k h
      s' k = showIndented k s
      t' k = showIndented k t

instance ShowIndented TObject where
  showIndented n pt@TObject {..} = printNonLeaf n pt [cs', a', t', s']
    where
      cs' k = concat (showIndented k <$> cs)
      a' k =
        case a of
          Opt2A p -> showIndented k p
          Opt2B p -> showIndented k p
      t' k = showIndented k t
      s' k = concatMap (showIndented k) s

instance ShowIndented TAbstraction where
  showIndented m pt@TAbstraction {..} = printNonLeaf m pt [as', t']
    where
      as' k = showIndented k as
      t' k = showIndented k t

instance ShowIndented TTail where
  showIndented m pt@TTail {..} = printNonLeaf m pt [os']
    where
      os' k = concat (showIndented k <$> os)

instance ShowIndented TApplication where
  showIndented m pt@TApplication {..} = printNonLeaf m pt [s', h', a1']
    where
      s' k = case s of
        Opt2A a -> showIndented k a
        Opt2B a -> showIndented k a
      h' k = showIndented k h
      a1' k = showIndented k a1

instance ShowIndented TApplication1 where
  showIndented m pt@TApplication1 {..} = printNonLeaf m pt [c']
    where
      c' k = showIndented k c

instance ShowIndented TApplication1Elem where
  showIndented m pt@TApplication1Elem {..} = printNonLeaf m pt [c1', ht', a']
    where
      c1' k = case c1 of
        Opt3A t -> showIndented k t
        Opt3B t -> showIndented k t
        Opt3C t -> showIndented k t
      ht' k = showIndented k ht
      a' k = showIndented k a

instance ShowIndented TMethod where
  showIndented n pt@TMethod {..} = printNonLeaf n pt [m']
    where
      m' k = case m of
        Opt2A t -> showIndented k t
        Opt2B t -> printTerminal k t

instance ShowIndented THas where
  showIndented m pt@THas {..} = printNonLeaf m pt [n']
    where
      n' k = showIndented k n

instance ShowIndented TAttributes where
  showIndented m pt@TAttributes {..} = printNonLeaf m pt [as']
    where
      as' k = concat (showIndented k <$> as)

instance ShowIndented TAbstractionTail where
  showIndented m pt@TAbstractionTail {..} = printNonLeaf m pt [e']
    where
      e' k =
        case e of
          Opt2A (a,b) ->
            showIndented k a <> (
                  case b of
                    Just b' ->
                      case b' of
                        Opt2A name -> showIndented k name
                        Opt2B t -> printTerminal k t
                    Nothing -> printNothing k
                )
          Opt2B h -> showIndented k h

instance ShowIndented THtail where
  showIndented m pt@THtail {..} = printNonLeaf m pt [t']
    where
      t' k = concat (f k <$> t)
      f k e =
        case e of
          Opt3A h -> showIndented k h
          Opt3B a -> showIndented k a
          Opt3C a -> showIndented k a

instance ShowIndented TLabel where
  showIndented m pt@TLabel {..} = printNonLeaf m pt [l']
    where
      l' k =
        case l of
          Opt2A t -> printTerminal k t
          Opt2B (n, t) ->
            showIndented k n <> showIndented k t

instance ShowIndented TFreeAttribute where
  showIndented m pt@TFreeAttribute {..} = printNonLeaf m pt [l']
    where
      l' k =
        case l of
          Opt3A t -> printTerminal k t
          Opt3B t -> showIndented k t
          Opt3C t -> showIndented k t

instance ShowIndented TDots where
  showIndented m t = printTerminal m t

instance ShowIndented TConst where
  showIndented m t = printTerminal m t

instance ShowIndented TSuffix where
  showIndented m pt@TSuffix {..} = printNonLeaf m pt [l', c']
    where
      l' k = showIndented k l
      c' k = printMaybe printTerminal k c

printTerminal :: (Data a, EpiAnn a) => Int -> a -> String
printTerminal m a = printf "%s%s %s %s\n" (tabs m) (show $ constructorName a) (show segment) (show num)
  where
    Ann {segment = segment, num = num} = get a

instance ShowIndented THead where
  showIndented m pt@THead {..} = printNonLeaf m pt [dots', t']
    where
      dots' k = printMaybe printTerminal k dots
      t' k =
        case t of
          Opt3A a -> printTerminal k a
          Opt3B a -> showIndented k a
          Opt3C a -> showIndented k a

instance ShowIndented THeadName where
  showIndented m pt@THeadName {..} = printNonLeaf m pt [name', c']
    where
      name' k = showIndented k name
      c' k = printMaybe printTerminal k c

instance ShowIndented TData where
  showIndented m pt@TData {..} = printNonLeaf m pt [d']
    where
      d' k =
        case d of
          Opt9A a -> showIndented k a
          Opt9B a -> showIndented k a
          Opt9C a -> showIndented k a
          Opt9D a -> showIndented k a
          Opt9E a -> showIndented k a
          Opt9F a -> showIndented k a
          Opt9G a -> showIndented k a
          Opt9H a -> showIndented k a
          Opt9I a -> showIndented k a

instance ShowIndented TBool where
  showIndented m pt@TBool {..} = printLeaf m pt b

instance ShowIndented TText where
  showIndented m pt@TText {..} = printLeaf m pt t

instance ShowIndented THex where
  showIndented m pt@THex {..} = printLeaf m pt h

instance ShowIndented TString where
  showIndented m pt@TString {..} = printLeaf m pt s

instance ShowIndented TFloat where
  showIndented m pt@TFloat {..} = printLeaf m pt f

instance ShowIndented TInt where
  showIndented m pt@TInt {..} = printLeaf m pt i

instance ShowIndented TBytes where
  showIndented m pt@TBytes {..} = printNonLeaf m pt [bs']
    where
      bs' k =
        case bs of
          Opt2A t -> showIndented k t
          Opt2B t -> printList (showIndented k) t

instance ShowIndented TChar where
  showIndented m pt@TChar {..} = printLeaf m pt c

instance ShowIndented TRegexBody where
  showIndented m pt@TRegexBody  {..} = printLeaf m pt b

instance ShowIndented TRegexSuffix where
  showIndented m pt@TRegexSuffix  {..} = printLeaf m pt s

instance ShowIndented TRegex where
  showIndented m pt@TRegex {..} = printNonLeaf m pt [r', s']
    where
      r' i = showIndented i rb
      s' i = showIndented i rs

instance ShowIndented TLineBytes where
  showIndented m pt@TLineBytes {..} = printNonLeaf m pt [bs']
    where
      bs' k = printList (showIndented k) bs

instance ShowIndented TByte where
  -- FIXME missing case
  showIndented m pt@TByte {..} = printLeaf m pt b
