{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances #-}
module PrettyPrintTree (printTree) where

import ParseEO

import qualified Data.List as DL(intercalate)
import           Text.Printf                (printf, PrintfType)
import Data.Data (Data(toConstr))


-- printing utilities

-- wrapper for printing nodes as indented

newtype Indented a = Indented a

instance ShowIndented a => Show (Indented a) where
  show (Indented x) = showIndented 0 x

printTree :: ShowIndented a => a -> String
printTree t = show (Indented t)

-- Print helpers

tab :: String
tab = "|  "

tabs :: Int -> String
tabs n = DL.intercalate "" (replicate n tab)

constructorName :: Data a => a -> String
constructorName n = show $ toConstr n

showHead :: (PrintfType t, Data a) => Int -> I a -> t
showHead n Node {..} = printf "%s%s %s %s" (tabs n) (tail $ constructorName node) (show pos) (show load)

nothing :: String
nothing = "Nothing"

printNonLeaf :: (Data a, Foldable t, Functor t) =>
  Int -> I a -> t (Int -> [Char]) -> [Char]
printNonLeaf n i l = showHead n i <> "\n" <> printList ($ n + 1) l

printLeaf :: (Show t, Data a) =>
  Int -> I a -> t -> [Char]
printLeaf n i d = showHead n i <>  " " <> show d <> "\n"

printMaybe :: (Int -> a -> String) -> Int -> Maybe a -> String
printMaybe f k a = maybe (printNothing k) (f k) a

printList :: (Foldable t, Functor t) => (a1 -> [a2]) -> t a1 -> [a2]
printList f t = foldl (<>) [] (f <$> t)

-- Node printers

instance ShowIndented a => ShowIndented (Maybe a) where
  showIndented m (Just a) = showIndented m a
  showIndented m Nothing = printNothing m

class ShowIndented a where
  showIndented :: Int -> a -> String

instance ShowIndented (I TProgram) where
  showIndented n i@Node {node = TProgram {..}} = printNonLeaf n i [l', m', o']
    where
      l' k = showIndented k l
      m' k = showIndented k m
      o' k = showIndented k o

instance ShowIndented (I TLicense) where
  showIndented n i@Node {node = TLicense {..}} = printNonLeaf n i [cs']
    where
      cs' k = foldl (<>) [] (showIndented k <$> cs)

instance ShowIndented (I TComment) where
  showIndented n i@Node {node = TComment {..}} = printLeaf n i c

instance ShowIndented (I TMetas) where
  showIndented n i@Node {node = TMetas {..}} = printNonLeaf n i [cs']
    where
      cs' k = foldl (<>) [] (showIndented k <$> ms)

printNothing :: Int -> String
printNothing n = tabs n <> nothing <> "\n"

instance ShowIndented (I TMeta) where
  showIndented n i@Node {node = TMeta {..}} = printNonLeaf n i [name', suff']
    where
      name' k = showIndented k name
      suff' k = showIndented k suff

instance ShowIndented (I TMetaSuffix) where
  showIndented n i@Node {node = TMetaSuffix {..}} = printLeaf n i s

instance ShowIndented (I TName) where
  showIndented m i@Node {node = TName {..}} = printLeaf m i n

instance ShowIndented (I TVarArg) where
  showIndented m i@Node {node = TVarArg {..}} = printLeaf m i n

instance ShowIndented (I TObjects) where
  showIndented n i@Node {node = TObjects {..}} = printNonLeaf n i [os']
    where
      os' k = foldl (<>) [] (showIndented k <$> os)

instance ShowIndented (I TObject) where
  showIndented n i@Node {node = TObject {..}} = printNonLeaf n i [cs', a', t']
    where
      cs' k = foldl (<>) [] (showIndented k <$> cs)
      a' k =
        case a of
          Opt2A p -> showIndented k p
          Opt2B p -> showIndented k p
      t' k = showIndented k t
      -- TODO print s
      s' = undefined 

instance ShowIndented (I TAbstraction) where
  showIndented m i@Node {node = TAbstraction {..}} = printNonLeaf m i [as', t']
    where
      as' k = showIndented k as
      t' k = showIndented k t

instance ShowIndented (I TTail) where
  showIndented m i@Node {node = TTail {..}} = printNonLeaf m i [os']
    where
      os' k = foldl (<>) [] (showIndented k <$> os)

instance ShowIndented (I TApplication) where
  showIndented m i@Node {node = TApplication {..}} = printNonLeaf m i [s', h', a1']
    where
      s' k = case s of
        Opt2A a -> showIndented k a
        Opt2B a -> showIndented k a
      h' k = showIndented k h
      a1' k = showIndented k a1

instance ShowIndented (I TApplication1) where
  showIndented m i@Node {node = TApplication1 {..}} = printNonLeaf m i [c']
    where
      c' k = showIndented k c

instance ShowIndented (I TApplication1Elem) where
  showIndented m i@Node {node = TApplication1Elem {..}} = printNonLeaf m i [c1', ht', a']
    where
      c1' k = case c1 of
        Opt3A t -> showIndented k t
        Opt3B t -> showIndented k t
        Opt3C t -> showIndented k t
      ht' k = showIndented k ht
      a' k = showIndented k a

instance ShowIndented (I TMethod) where
  showIndented n i@Node {node = TMethod {..}} = printNonLeaf n i [m']
    where
      m' k = case m of
        Opt2A t -> showIndented k t
        Opt2B t -> printTerminal k t

instance ShowIndented (I THas) where
  showIndented m i@Node {node = THas {..}} = printNonLeaf m i [n']
    where
      n' k = showIndented k n

instance ShowIndented (I TAttributes) where
  showIndented m i@Node {node = TAttributes {..}} = printNonLeaf m i [as']
    where
      as' k = foldl (<>) [] (showIndented k <$> as)

instance ShowIndented (I TTerminal) where
  showIndented m t = printTerminal m t

instance ShowIndented (I TAbstractionTail) where
  showIndented m i@Node {node = TAbstractionTail {..}} = printNonLeaf m i [e']
    where
      e' k =
        case e of
          Opt2A (a,b) ->
            showIndented k a <> (
                  case b of
                    Just b' ->
                      case b' of
                        Opt2A name -> showIndented k name
                        Opt2B t -> showIndented k t
                    Nothing -> printNothing k
                )
          Opt2B h -> showIndented k h

instance ShowIndented (I THtail) where
  showIndented m i@Node {node = THtail {..}} = printNonLeaf m i [t']
    where
      t' k = foldl (<>) [] (f k <$> t)
      f k e =
        case e of
          Opt3A h -> showIndented k h
          Opt3B a -> showIndented k a
          Opt3C a -> showIndented k a

instance ShowIndented (I TLabel) where
  showIndented m i@Node {node = TLabel {..}} = printNonLeaf m i [l']
    where
      l' k =
        case l of
          Opt2A t -> printTerminal k t
          Opt2B (n, t) ->
            showIndented k n <> showIndented k t

instance ShowIndented (I TFreeAttribute) where
  showIndented m i@Node {node = TFreeAttribute {..}} = printNonLeaf m i [l']
    where
      l' k =
        case l of
          Opt3A t -> showIndented k t
          Opt3B t -> showIndented k t
          Opt3C t -> showIndented k t

instance ShowIndented (I TDots) where 
  showIndented m t = printTerminal m t

instance Show TDots where
  show t = constructorName t

instance Show TConst where
  show t = constructorName t

instance ShowIndented (I TConst) where
  showIndented m t = printTerminal m t
instance ShowIndented (I TSuffix) where
  showIndented m i@Node {node = TSuffix {..}} = printNonLeaf m i [l', c']
    where
      l' k = showIndented k l
      c' k = printMaybe printTerminal k c

printTerminal :: (Show a) => Int -> I a -> String
printTerminal m Node {..} = printf "%s%s %s %s\n" (tabs m) (show node) (show pos) (show load)

instance ShowIndented (I THead) where
  showIndented m i@Node {node = THead {..}} = printNonLeaf m i [dots', t']
    where
      dots' k = printMaybe printTerminal k dots
      t' k =
        case t of
          Opt3A a -> printTerminal k a
          Opt3B a -> showIndented k a
          Opt3C a -> showIndented k a

instance ShowIndented (I THeadName) where
  showIndented m i@Node {node = THeadName {..}} = printNonLeaf m i [name', c']
    where
      name' k = showIndented k name
      c' k =
        case c of
          Opt2A t -> printTerminal k t
          Opt2B t -> printMaybe printTerminal k t

instance ShowIndented (I TData) where
  showIndented m i@Node {node = TData {..}} = printNonLeaf m i [d']
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

instance ShowIndented (I TBool) where
  showIndented m i@Node {node = TBool {..}} = printLeaf m i b

instance ShowIndented (I TText) where
  showIndented m i@Node {node = TText {..}} = printLeaf m i t

instance ShowIndented (I THex) where
  showIndented m i@Node {node = THex {..}} = printLeaf m i h

instance ShowIndented (I TString) where
  showIndented m i@Node {node = TString {..}} = printLeaf m i s

instance ShowIndented (I TFloat) where
  showIndented m i@Node {node = TFloat {..}} = printLeaf m i f

instance ShowIndented (I TInt) where
  showIndented m i@Node {node = TInt {..}} = printLeaf m i s

instance ShowIndented (I TBytes) where
  showIndented m i@Node {node = TBytes {..}} = printNonLeaf m i [bs']
    where
      bs' k =
        case bs of
          Opt2A t -> showIndented k t
          Opt2B t -> printList (showIndented k) t

instance ShowIndented (I TChar) where
  showIndented m i@Node {node = TChar {..}} = printLeaf m i c

instance ShowIndented (I TRegex) where
  showIndented m i@Node {node = TRegex {..}} = printLeaf m i (r <> " " <> suff)

instance ShowIndented (I TLineBytes) where
  showIndented m i@Node {node = TLineBytes {..}} = printNonLeaf m i [bs']
    where
      bs' k = printList (showIndented k) bs

instance ShowIndented (I TByte) where
  showIndented m i@Node {node = TByte {..}} = printLeaf m i b
