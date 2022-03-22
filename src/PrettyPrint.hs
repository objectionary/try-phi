{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings     #-}


module PrettyPrint where

import ParseEO

import qualified Data.List as DL(intercalate)
import           Text.Printf                (printf, PrintfType)
import Data.Data (Data(toConstr))

tab :: String
tab = "|  "

type TabNumber = Int

tabs :: Int -> String
tabs n = DL.intercalate "" (replicate n tab)

cName :: Data a => a -> String
cName n = show $ toConstr n

showHead :: (PrintfType t, Data a) => Int -> I a -> t
showHead n Node {..} = printf "%s%s %s %s" (tabs n) (tail $ cName node) (show pos) (show load)

nothing :: String
nothing = "Nothing"

printTree :: I TProgram -> String
printTree p = printProgram 0 p

-- | for non-leaf nodes
printNonLeaf :: (Data a, Foldable t, Functor t) =>
  Int -> I a -> t (Int -> [Char]) -> [Char]
printNonLeaf n i l = showHead n i <> "\n" <> printList ($ n + 1) l

-- | for leaf nodes
printLeaf :: (Show t, Data a) =>
  Int -> I a -> t -> [Char]
printLeaf n i d = showHead n i <>  " " <> show d <> "\n"

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
    -- TODO
    s' = undefined 

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


-- showHead n Node {..} = printf "%s%s %s %s" (tabs n) (cName node) (show pos) (show load)
-- TODO
printTerminal :: Int -> I TTerminal -> String
printTerminal m i@Node {..} = printf "%s%s %s %s\n" (tabs m) (show node) (show pos) (show load)
-- printLeaf m i t

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
        Opt2A t -> printByte k t
        Opt2B t -> printList (printLineBytes k) t

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
