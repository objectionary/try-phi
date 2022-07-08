{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module ToTerm
  ( -- composeProgram,
    -- getTermProgram,
    Term (..),
    -- K,
    DataValue (..),
    Ann (..),
    DByte (..),
    AbstrQuestion(..),
    DRegexBody (..),
    DRegexSuffix (..),
    DLineBytes (..),
    HasName (..),
    AttachedName (..),
    AttachedOrArgument (..),
    Abstraction (..),
    ToTerm.Label (..),
    MethodName (..),
    Head (..),
    HeadTerminal (..),
    HeadName (..),
    LetterName (..),
    Modifier (..),
    SuffixName (..),
    AttachedOrArgName,
    -- initAnn,
    Options2 (..),
    Options3 (..),
    getTermProgram
  )
where

import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Maybe (isJust)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)
import ParseEO as P
  ( Options2 (..),
    Options3 (..),
    Options9 (..),
    TAbstrQuestion (..),
    TAbstraction (..),
    TAbstractionTail (..),
    TApplication (..),
    TApplication1 (..),
    TApplication1Elem (..),
    TAttributes (..),
    TBool (..),
    TByte (..),
    TBytes (..),
    TChar (..),
    TData (..),
    TFloat (..),
    TFreeAttribute (..),
    THas (..),
    THead (..),
    THeadModifier (..),
    THeadName (..),
    THeadTerminal (..),
    THex (..),
    THtail (..),
    TInt (..),
    TLabel (..),
    TLabelTerminal (..),
    TLineBytes (..),
    TMethod (..),
    TMethodTerminal (..),
    TName (..),
    TObject (..),
    TObjectTail (..),
    TObjects (..),
    TProgram (..),
    TRegex (..),
    TRegexBody (..),
    TRegexSuffix (..),
    TString (..),
    TSuffix (..),
    TTail (..),
    TText (..),
    TVarArg (..),
  )
import qualified ParseEO as P (Ann (..), EpiAnn (..))
import PrettyPrintTree ()
import ToTermTH

-- type Id = Int

-- newtype Ann = Ann {id :: Int} deriving (Data, Eq, Ord, Hashable, Generic)

-- data Ann a b = Ann {term::a, ann::b} deriving (Data)
-- data Annotation = IDs {treeId::Maybe Id, runtimeId::Maybe Id}  deriving (Data)

-- | shorthand for annotation

-- type a = Ann a Annotation

-- TODO convert inverse dot to dot

-- Attribute names
-- if a constructor is composite
-- its argument types should be declared separately
-- its argument types should be annotated

-- if a type has many constructors,
-- it will be annotated outside

data Label
  = LName {n :: Text, ann :: Ann}
  | LAt {ann :: Ann}
  | LVarArg {t :: Text, ann :: Ann}
  deriving (Data)

data LetterName = LetterName {n :: Text, ann :: Ann}
  deriving (Data)

data Modifier
  = MCopy {ann :: Ann}
  | MInverseDot {ann :: Ann}
  deriving (Data)

data HeadName = HeadName {n :: LetterName, m :: Maybe Modifier, ann :: Ann} deriving (Data)

-- TODO Question is not a terminal

data MethodName
  = MName {n :: Text, ann :: Ann}
  | MRho {ann :: Ann}
  | MAt {ann :: Ann}
  | MVertex {ann :: Ann}
  deriving (Eq, Ord, Generic, Data, Hashable)

-- AttachedOrArg
-- Unpacked can be ^.x, not necessarily a name

-- | any elementary expressions that can stand in the head, e.g.:
--
-- Reserved name `@` in `@.c`
--
-- Name `a` in `a.b`
--
-- Number `3` in `3.a`
data Head = Head {h :: Options3 HeadTerminal HeadName DataValue, unpacked :: Bool, ann :: Ann} deriving (Data)

data DByte = DByte {byte :: Integer, ann :: Ann} deriving (Data)

data DLineBytes = DLineBytes {bs :: [DByte], ann :: Ann} deriving (Data)

data DRegexBody = DRegexBody {b :: Text, ann :: Ann} deriving (Data)

data DRegexSuffix = DRegexSuffix {s :: Text, ann :: Ann} deriving (Data)

data DataValue
  = DBool {b :: Bool, ann :: Ann}
  | DBytes {bs :: Options2 DByte [DLineBytes], ann :: Ann}
  | DChar {c :: Char, ann :: Ann}
  | DFloat {f :: Scientific, ann :: Ann}
  | DHex {h :: Integer, ann :: Ann}
  | DInt {i :: Integer, ann :: Ann}
  | DRegex {rb :: DRegexBody, rs :: DRegexSuffix, ann :: Ann}
  | DString {s :: Text, ann :: Ann}
  | -- TODO save indentation of closing quotes
    DText {t :: Text, ann :: Ann}
  deriving (Data)

-- TODO define when to throw exceptions
-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

data HasName
  = HName {t :: Text, ann :: Ann}
  | HAt {ann :: Ann}
  deriving (Data)

newtype AbstrQuestion = AbstrQuestion {ann :: Ann} deriving (Data)

type ImportedName = Options2 LetterName AbstrQuestion

data AttachedName = AttachedName {a :: SuffixName, imported :: Maybe ImportedName, ann :: Ann}
  deriving (Data)

type AttachedOrArgName = Options2 AttachedName (Maybe HasName)

data AttachedOrArgument = AttachedOrArgument {t :: Term, a :: [AttachedOrArgName], ann :: Ann} deriving (Data)

{-
Not yet a full-fledged abstract attribute
Might not have a name
or attributes that will come later in the next lines

-- TODO better type

-- IDK
should it be different from AttachedOrArg?
If abstractionTail has attributes in parentheses, we can construct an AttachedOrArg (inline object)
Probably, such object is always anonymous

otherwise, Abstraction should store AttachedName
-}
data Abstraction = Abstraction {attrs :: [Label], t :: Maybe AbstractionTail, ann :: Ann} deriving (Data)

{-
-- TODO Don't distinguish between them?
-}
-- type AttachedOrArg = Options2 AttachedOrArg AttachedOrArg

{-
For cases like
[a] (3 > b) c
here, the anonymous abstract object is applied to c
-}
type AbstractionTail = Options2 AttachedName [AttachedOrArgument]

data HeadTerminal
  = HeadRoot {ann :: Ann}
  | HeadAt {ann :: Ann}
  | HeadRho {ann :: Ann}
  | HeadXi {ann :: Ann}
  | HeadSigma {ann :: Ann}
  | HeadStar {ann :: Ann}
  deriving (Data)

data Term
  = App {t :: AttachedOrArgument, args :: [AttachedOrArgument], ann :: Ann}
  | Obj {freeAttrs :: [Label], attrs :: [AttachedOrArgument], ann :: Ann}
  | Dot {t :: AttachedOrArgument, attr :: [MethodName], ann :: Ann}
  | -- for cases like just `^` or `$`
    -- it doesn't need body
    HeadTerm {n :: Maybe Int, a :: Maybe Head, ann :: Ann}
  deriving (Data)

-- newtype ReturnValue = ReturnValue {t :: Term} deriving (Data)

-- newtype MyState = MyState {termId :: Int} deriving (Data)

data SuffixName = SuffixName {n :: Label, isConst :: Bool, ann :: Ann} deriving (Data)

getId :: (EpiAnn a) => a -> Int
getId a = num $ get a

-- | annotate a node with tree node id
dec :: (P.EpiAnn a, EpiAnn b) => a -> b -> b
dec a b = b'
  where
    P.Ann {num = num} = P.get a
    b' = modify (const Ann {num = num}) b

$( genEpiN
     [
       ''Abstraction,
       ''AbstrQuestion,
       ''AttachedName,
       ''AttachedOrArgument,
       ''DByte,
       ''DLineBytes,
       ''DRegexBody,
       ''DRegexSuffix,
       ''DataValue,
       ''HasName,
       ''Head,
       ''HeadName,
       ''HeadTerminal,
       ''Label,
       ''LetterName,
       ''MethodName,
       ''Modifier,
       ''SuffixName,
       ''Term
     ]
 )

-- do
-- t1 <- t
-- MyState tId <- get
-- put (MyState (tId + 1))
-- return _

-- Ann {term = t1, ann = IDs {runtimeId = Just tId, treeId = Just (getId n)}}

-- |
-- convert parsed tree into annotated terms
getTermProgram :: TProgram -> Term
getTermProgram = composeProgram

composeProgram :: TProgram -> Term
composeProgram TProgram {..} =
  composeObjects o

-- toTermMetas :: TMetas -> (TMetas)
-- toTermMetas pt@TMetas {..} = dec pt $ do
--     cs' <- mapM toTermMeta ms
--     return $ TMetas cs'

-- toTermMeta :: TMeta -> (TMeta)
-- toTermMeta pt@TMeta {..} = dec pt $ do
--     name' <- composeLetterName name
--     suff' <- toTermMaybe toTermMetaSuffix suff
--     return $ TMeta name' suff'

-- toTermMetaSuffix :: TMetaSuffix -> (TMetaSuffix)
-- toTermMetaSuffix pt@Node {..} = dec pt $ return node

composeLetterName :: TName -> LetterName
composeLetterName m@TName {..} = dec m $ LetterName {n}

-- should produce an object
composeObjects :: TObjects -> Term
composeObjects pt@TObjects {..} = dec pt ret
  where
    os' = composeObject <$> os
    ret = Obj {freeAttrs = [], attrs = os'}

{-
-- TODO also handle cases like
  [x] a > b
    .c
  .d > e
-}

-- FIXME use num from objecttail
appendObjectTail :: AttachedOrArgument -> TObjectTail -> AttachedOrArgument
appendObjectTail p pt@TObjectTail {..} = ret
  where
    m1 = applyDot p m
    m2 = appendMaybeTail m1 composeHtail h
    m3 = appendMaybeSuffix m2 s Nothing
    ret = appendMaybeTail m3 composeTail t

composeObject :: TObject -> AttachedOrArgument
composeObject pt@TObject {..} = dec pt ret
  where
    a' =
      case a of
        Opt2A p -> composeAbstraction p
        Opt2B p -> composeApplication p
    t' = appendMaybeTail a' composeTail t
    ret = foldl appendObjectTail t' s

-- TODO correctly handle tail, not ignore it

composeAbstraction :: TAbstraction -> AttachedOrArgument
composeAbstraction pt@TAbstraction {..} = dec pt ret
  where
    as' = composeFreeAttributes as
    t' =
      case t of
        Just t1 -> Just $ composeAbstractionTail t1
        Nothing -> Nothing

    -- FIXME name here
    m name ks = (\t1 -> AttachedOrArgument {t = t1, a = name}) $ dec pt Obj {freeAttrs = as', attrs = ks}

    ret =
      case t' of
        Just p ->
          case p of
            Opt2A a -> m [Opt2A a] []
            Opt2B b -> m [] b
        Nothing ->
          m [] []

-- | produce a list of arguments
composeTail :: TTail -> [AttachedOrArgument]
composeTail TTail {..} = composeObject <$> os

composeApplication :: TApplication -> AttachedOrArgument
composeApplication TApplication {..} = ret
  where
    s' =
      case s of
        Opt2A a -> attachHead a
        -- application in parentheses
        Opt2B a -> composeApplication a
    h' = appendMaybeTail s' composeHtail h
    ret = composeApplication1 h' a1

composeApplication1 :: AttachedOrArgument -> TApplication1 -> AttachedOrArgument
composeApplication1 t TApplication1 {..} =
  case c of
    Just x -> composeApplication1Elem t x
    Nothing -> t

-- TODO use runtime ids from state, not Nothing?
-- initAnn :: a -> a
-- initAnn x = Ann {term = x, ann = IDs {treeId = Nothing, runtimeId = Nothing}}

-- | apply term to a list of arguments
--
-- if an object, append the arguments to its list
--
-- if an application, aadd the arguments to its list
--
-- otherwise, produce a new term applied to this list
applyTail :: AttachedOrArgument -> [AttachedOrArgument] -> AttachedOrArgument
applyTail pt@AttachedOrArgument {t=t1} ts = ret
  where
    ret =
      case ts of
        [] -> pt
        _ ->
          (\z -> pt {t = z} :: AttachedOrArgument) $
            case t1 of
              App {..} -> App {t = t, args = args <> ts}
              Obj {..} -> Obj {freeAttrs = freeAttrs, attrs = attrs <> ts}
              Dot {..} -> App {t = AttachedOrArgument {t = t1, a = []}, args = ts}
              HeadTerm {..} -> App {t = AttachedOrArgument {t = t1, a = []}, args = ts}

applyDot :: AttachedOrArgument -> TMethod -> AttachedOrArgument
applyDot pt@AttachedOrArgument {t = t2, a = a1} b = ret
  where
    m = composeMethod b
    ret =
      case (t2, a1) of
        -- if unnamed dot term, put new method name inside
        (Dot {..}, []) -> (\z -> t {t = z} :: AttachedOrArgument) $ Dot {t, attr = attr <> [m]}
        -- otherwise, compose a new anonymous dot term with such method name
        _ -> (\z -> AttachedOrArgument {t = z, a = []}) $ Dot {t = pt, attr = [m]}

composeApplication1Elem :: AttachedOrArgument -> TApplication1Elem -> AttachedOrArgument
composeApplication1Elem t TApplication1Elem {..} = ret
  where
    c1' =
      case c1 of
        -- append method name to an application
        Opt3A b -> applyDot t b
        -- append optional argument name to a term
        -- TODO don't append has twice
        Opt3B b -> appendHas t b
        -- no imported name since it's an application
        Opt3C b -> appendMaybeSuffix t (Just b) Nothing

    ht' = appendMaybeTail c1' composeHtail ht
    ret = composeApplication1 ht' a

-- | produce annotated method name
composeMethod :: TMethod -> MethodName
composeMethod pt@TMethod {..} = dec pt ret
  where
    ret =
        case m of
          Opt2A TName {..} -> MName {n}
          Opt2B t1 -> dec t1 $
            case t1 of
              TMethodRho {} -> MRho {}
              TMethodVertex {} -> MVertex {}
              TMethodAt {} -> MAt {}

-- | append optional argument name to a term
-- FIXME somehow refer to a super node (to these two)
appendHas :: AttachedOrArgument -> THas -> AttachedOrArgument
appendHas pt@AttachedOrArgument {a = a1} ps@THas {..} = ret
  where
    TName {n = n1} = n
    h = dec ps (HName {t = n1})
    -- TODO don't append name?
    ret = pt {a = a1 <> [Opt2B (Just h)]} :: AttachedOrArgument

-- | append suffix name with possibly imported name to a term
appendMaybeSuffix :: AttachedOrArgument -> Maybe TSuffix -> Maybe ImportedName -> AttachedOrArgument
appendMaybeSuffix pt@AttachedOrArgument {a = a1, t = t} ms i = ret
  where
    ret =
      case ms of
        Nothing -> pt
        Just ms' -> ret'
          where
            s' = composeSuffix ms'
            -- FIXME refer to super node
            a' = dec ms' AttachedName {a = s', imported = i}
            ret' :: AttachedOrArgument
            ret' = (pt {a = a1 <> [Opt2A a']})

-- | produce a list of annotated labels
composeFreeAttributes :: TAttributes -> [Label]
composeFreeAttributes TAttributes {..} =
  composeFreeAttribute <$> as

-- | produce a free attribute as a label
composeFreeAttribute :: TFreeAttribute -> Label
composeFreeAttribute pt@TFreeAttribute {..} = dec pt ret
  where
    ret =
      case l of
        Opt3A LabelAt {..} -> LAt {}
        Opt3B TName {..} -> LName {n = n}
        Opt3C TVarArg {..} -> LVarArg {t = n}

composeAbstrQuestion :: TAbstrQuestion -> AbstrQuestion
composeAbstrQuestion pt@TAbstrQuestion {..} = dec pt AbstrQuestion {}

composeAbstractionTail :: TAbstractionTail -> AbstractionTail
composeAbstractionTail TAbstractionTail {..} =
  case e of
    Opt2A (a, b) -> ret
      where
        a1 = composeSuffix a
        b1 =
          case b of
              Just b' ->
                Just
                  $ case b' of
                    Opt2A c -> Opt2A $ composeLetterName c
                    Opt2B c -> Opt2B $ composeAbstrQuestion c
              Nothing -> Nothing
        ret = (Opt2A AttachedName {a = a1, imported = b1})
    Opt2B h -> Opt2B $ composeHtail h

-- | compose an anonymous expression for head
attachHead :: THead -> AttachedOrArgument
attachHead pt@THead {..} = ret
  where
    a' = composeHead pt
    b = dec pt HeadTerm {n = Nothing, a = Just a'}
    ret = AttachedOrArgument {t = b, a = []}

appendMaybeTail :: AttachedOrArgument -> (a -> [AttachedOrArgument]) -> Maybe a -> AttachedOrArgument
appendMaybeTail a f h = applyTail a $ maybe [] f h

-- | compose list of possibly attached arguments
composeHtail :: THtail -> [AttachedOrArgument]
composeHtail THtail {..} = ret
  where
    f e =
          case e of
            Opt3A a -> attachHead a
            -- it's an application in parentheses
            Opt3B a -> composeApplication a
            -- TODO add case for explicit construction of Opts
            Opt3C a -> composeAbstraction a
    ret = f <$> t

{-
-- TODO
[]
  Q.x.f.d Q Q
  &.@.< > t
  ^.@.hey > you...
varargs can be in label

-}

composeLabel :: TLabel -> Label
composeLabel pt@TLabel {..} = dec pt ret
  where
    ret =
      case l of
        Opt2A _ -> LAt {}
        Opt2B (TName {..}, t) ->
          case t of
            Just _ -> LVarArg {t = n}
            Nothing -> LName {n = n}

composeSuffix :: TSuffix -> SuffixName
composeSuffix pt@TSuffix {..} = dec pt ret
  where
    l' = composeLabel l
    c' = Data.Maybe.isJust c
    ret = SuffixName {n = l', isConst = c'}

composeHeadTerminal :: THeadTerminal -> HeadTerminal
composeHeadTerminal pt = dec pt $
  case pt of
    THeadRoot {} -> HeadRoot {}
    THeadAt {} -> HeadAt {}
    THeadRho {} -> HeadRho {}
    THeadXi {} -> HeadXi {}
    THeadSigma {} -> HeadSigma {}
    THeadStar {} -> HeadStar {}


composeHead :: THead -> Head
composeHead pt@THead {..} = dec pt ret
  where
    d = Data.Maybe.isJust dots
    t' =
      case t of
        Opt3A a -> Opt3A $ composeHeadTerminal a
        Opt3B a -> Opt3B $ composeHeadName a
        Opt3C a -> Opt3C $ composeData a
    ret = Head {h = t', unpacked = d}

composeHeadName :: THeadName -> HeadName
composeHeadName pt@THeadName {..} = dec pt ret
  where
    TName {n = n} = name
    hn = dec name (LetterName {n = n})
    c' =
      case c of
        Just n1 ->
          case n1 of
            THeadDot {..} -> Just $ dec n1 MInverseDot {}
            THeadCopy {..} -> Just $ dec n1 MCopy {}
        Nothing -> Nothing
    ret = HeadName {n = hn, m = c'}

composeData :: TData -> DataValue
composeData pt@TData {..} = dec pt $
  case d of
    Opt9A a -> composeBool a
    Opt9B a -> composeText a
    Opt9C a -> composeHex a
    Opt9D a -> composeString a
    Opt9E a -> composeFloat a
    Opt9F a -> composeInt a
    Opt9G a -> composeBytes a
    Opt9H a -> composeChar a
    Opt9I a -> composeRegex a

composeBytes :: TBytes -> DataValue
composeBytes pt@TBytes {..} = dec pt (DBytes {bs = bs'})
  where
    bs' =
      case bs of
        Opt2A t -> Opt2A $ composeByte t
        Opt2B t -> Opt2B $ composeLineBytes <$> t

composeBool :: TBool -> DataValue
composeBool pt@TBool {..} = dec pt DBool {b}

composeText :: TText -> DataValue
composeText pt@TText {..} = dec pt DText {t}

composeHex :: THex -> DataValue
composeHex pt@THex {..} = dec pt DHex {h}

composeString :: TString -> DataValue
composeString pt@TString {..} = dec pt DString {s}

composeFloat :: TFloat -> DataValue
composeFloat pt@TFloat {..} = dec pt DFloat {f}

composeChar :: TChar -> DataValue
composeChar pt@TChar {..} = dec pt DChar {c}

composeRegexBody :: TRegexBody -> DRegexBody
composeRegexBody pt@TRegexBody {..} = dec pt DRegexBody {b}

composeRegexSuffix :: TRegexSuffix -> DRegexSuffix
composeRegexSuffix pt@TRegexSuffix {..} = dec pt DRegexSuffix {s}

composeRegex :: TRegex -> DataValue
composeRegex pt@TRegex {..} = dec pt ret
  where
    rb' = composeRegexBody rb
    rs' = composeRegexSuffix rs
    ret = DRegex {rb = rb', rs = rs'}

composeInt :: TInt -> DataValue
composeInt pt@TInt {..} = dec pt DInt {i}

composeLineBytes :: TLineBytes -> DLineBytes
composeLineBytes pt@TLineBytes {..} = dec pt ret
  where
    bs' = composeByte <$> bs
    ret = DLineBytes {bs = bs'}

composeByte :: TByte -> DByte
composeByte pt@TByte {..} = dec pt DByte {byte = b}

{-
-- IDK
does `c` in `a b:c` refer to `b` or to `a b`?
Let us make `c` refer to `a b`
To make it refer to `b` only, we can write `a (b:c)`

We get some term and apply a modifier to it to get
a.b, a:b, a > b

Next, there can be a htail.
The meaning of the final expression depends on the modifier

a.b c
is an ordinary application

a:b c
doesn't make sense since
  if it's an argument list where a has an optional name, it won't have
    a term to apply this list to due to left associativity of modifiers
  if it's an application, why write an optional name
    if `a` isn't applied to some term directly?

-- IDK
I think a `:` modifier refers to the last expression in htail

a:b
`b` refers to `a`

(a > b):c
`c` refers to `(a > b)`

a b:c
`c` refers to `b`?

[c] > a

[c]
  a b:c

a > b c
doesn't really make sense
  for the aforementioned reasons

However, if htail is empty in the last expression, e.g.
a > b

This means that we reached the end of the application chain
and can return this named term

-- IDwhat means method after suffix

-}

{-
-- INFO
Anywhere a new name Datas up after the > symbol,
it is a declaration of a new attribute in the nearest object abstraction.
-}

{-
list of free attributes and the name of abstraction can be of form

[a b] (a > b) (c > d)
  e > g
  f > h

For now, we just append this tail (with attributes `g` and `h`)
to the inline list (with attributes `b` and `d`)

-- IDK
[a b] (a > b) (c > d) e (f > g)
Does it mean that `[a b] (a > b) (c > d)` is applied to `e` and `(f > g)`?

-- TODO
probably need to find the first unattached attribute and suppose that
it's the first argument of this object

-}

{-
head
  is prepended by a locator to produce a term
  need a term to later be accessed by a dot
  assume that data is not directly accessible in the program and is located

head can be stored inside `AttachedOrArg` without a name
  its term can always be extracted
-- TODO maybe need a separate type for head?

a method `c` can be applied on application like this

a b.c

or on an expression like

a (a:b).c

and on something named also

(a > b).c
-}

{-
-- IDK
What's the meaning of `(a > a).a`?

Is it the same as
a > a
.a
?

Should we allow it or let the expressions in parentheses only for grouping?
(a b c)
-}

{-
In context of application tail,
attributes can have
  no name
  `has` name
  `suffix` name
  `args` name
-}
