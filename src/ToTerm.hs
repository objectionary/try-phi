{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

{-# LANGUAGE DuplicateRecordFields #-}

{-# LANGUAGE RecordWildCards       #-}

module ToTerm(
  toTermProgram,
  getTermProgram,
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
  ToTerm.Label(..),
  MethodName(..),
  Head(..),
  HeadName(..),
  LetterName(..),
  Modifier(..),
  SuffixName(..),
  AttachedOrArgName
  ) where


import           Control.Monad.State        (get, put, when)
import           Control.Monad.State.Strict (State, evalState)
import           Data.Hashable              (Hashable)
import           Data.Maybe                 (isJust)
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import ParseEO as PEO
    ( TText(TText),
      TName(TName),
      THex(THex),
      TFloat(TFloat),
      TInt(TInt),
      TString(TString),
      TChar(TChar),
      TBool(TBool),
      TBytes(..),
      TLineBytes(..),
      TByte(TByte),
      TRegex(..),
      TRegexSuffix(..),
      TRegexBody(..),
      TData(..),
      Options9(..),
      THas(..),
      THeadModifier(HeadCopy, HeadDot),
      THeadName(..),
      THeadTerminal,
      THead(..),
      THtail(..),
      TApplication1Elem(..),
      TApplication1(..),
      TApplication(..),
      TMethod(..),
      TMethodTerminal(MethodAt, MethodRho, MethodVertex),
      TSuffix(..),
      TTail(..),
      TLabelTerminal(LabelAt),
      TLabel(..),
      TFreeAttribute(..),
      TVarArg(TVarArg),
      TAttributes(..),
      TAbstrQuestion,
      TAbstractionTail(..),
      TAbstraction(..),
      TObject(..),
      TObjects(..),
      TProgram(..),
      Options3(..),
      Options2(..),
      Node(..),
      Load(Load),
      I,
      )
import PrettyPrintTree ()


type Id = Int
data Ann a b = Ann {term::a, ann::b} deriving (Show)
data Annotation = IDs {treeId::Maybe Id, runtimeId::Maybe Id}  deriving (Show)
type K a = Ann a Annotation

-- TODO convert inverse dot to dot

-- Attribute names
-- if a constructor is composite
  -- its argument types should be declared separately
  -- its argument types should be annotated

-- if a type has many constructors,
  -- it will be annotated outside

data Label =
    LName Text
  | LAt
  | LVarArg Text
  deriving (Show)


newtype LetterName = LetterName Text  deriving (Show)
data Modifier = MCopy | MInverseDot  deriving (Show)
data HeadName = HeadName {n::K LetterName, m::Maybe Modifier}  deriving (Show)

-- TODO Question is not a terminal

data MethodName =
    MName Text
  | MRho
  | MAt
  | MVertex
  deriving (Eq, Ord, Generic, Show, Hashable)

-- AttachedOrArg
-- Unpacked can be ^.x, not necessarily a name

instance (Show a, Show b, Show c) => Show (Options3 a b c) where
  show (Opt3A a) = show a
  show (Opt3B a) = show a
  show (Opt3C a) = show a

instance (Show a, Show b) => Show (Options2 a b) where
  show (Opt2A a) = show a
  show (Opt2B a) = show a


{- | any elementary expressions that can stand in the head, e.g.:
Reserved name @ in
  @.c

Name a in
  a.b

Number 3 in
  3.a

-}
data Head = Head {h::Options3 (K THeadTerminal) (K HeadName) (K DataValue), unpacked::Bool}  deriving (Show)

newtype DByte = DByte {byte::Integer}  deriving (Show)
newtype DLineBytes = DLineBytes {bs :: [K DByte]}  deriving (Show)

newtype DRegexBody = DRegexBody {b::Text} deriving (Show)
newtype DRegexSuffix = DRegexSuffix {s::Text} deriving (Show)

data DataValue
  = DBool Bool
  | DBytes (Options2  (K DByte) [K DLineBytes])
  | DChar Char
  | DFloat Scientific
  | DHex Integer
  | DInt Integer
  | DRegex (K DRegexBody) (K DRegexSuffix)
  | DString Text
  -- TODO save indentation of closing quotes
  | DText Text
 deriving (Show)

-- TODO define when to throw exceptions
-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

{-
-- stores names of abstract attributes
-}
-- data AttachedName = AttachedName {a::SuffixName, imported::Maybe (Options2 (K LetterName) (K TAbstrQuestion))}  deriving (Show)

{-
When an object term is ready, it becomes AttachedOrArg
-}
-- data AttachedOrArg = AttachedOrArg {t::K Term, a::Maybe AttachedName}  deriving (Show)


-- data Attached = Attached {t :: K Term, a::AttachedName}

-- data UnAttached = UnAttached {t :: K Term, a::Maybe (K HasName)}

data HasName = HName Text | HAt  deriving (Show)

data AttachedName = AttachedName {a::SuffixName, imported::Maybe (Options2 (K LetterName) (K TAbstrQuestion))} deriving (Show)

type AttachedOrArgName = Options2 AttachedName (Maybe (K HasName))

data AttachedOrArg = AttachedOrArg {t::K Term, a :: [AttachedOrArgName]} deriving (Show)

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
data Abstraction = Abstraction {attrs :: [K Label], t::Maybe AbstractionTail}  deriving (Show)

{-
-- TODO Don't distinguish between them?
-}
-- type AttachedOrArg = Options2 AttachedOrArg AttachedOrArg

{-
For cases like
[a] (3 > b) c
here, the anonymous abstract object is applied to c
-}
type AbstractionTail = Options2 AttachedName [AttachedOrArg]

-- data Method = 

data Term
  = App {t::AttachedOrArg, args::[AttachedOrArg]}
  | Obj {freeAttrs::[K Label], args::[AttachedOrArg]}
  | Dot {t::AttachedOrArg, attr::K MethodName}
  -- for cases like just `^` or `$`
  -- it doesn't need body
  | HeadTerm {n::Maybe Int, a::Maybe (K Head)}
  deriving (Show)

data ReturnValue = ReturnValue {t::K Term}  deriving (Show)
data MyState = MyState {termId :: Int}  deriving (Show)
data SuffixName = SuffixName {n::K Label, isConst::Bool}  deriving (Show)

{-
In context of application tail,
attributes can have
  no name
  `has` name
  `suffix` name
  `args` name

So, application arguments should have `Maybe (Options2 AttachedName (K HasName))` names
However, application expressions cannot give `AttachedName`,
so we use just `AppName`
-}
-- type AppName = Options2 SuffixName (K HasName)

{-
when an application term is ready, it becomes AttachedOrArg
-}
-- data AttachedOrArg = AttachedOrArg {t::K Term, a :: Maybe AppName}  deriving (Show)

-- dec::I a -> (I a -> MapElement) -> ReturnValue
-- dec n m p = do
--     t <- p
    -- MyState {i=i1, m=m1} <- get
    -- put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    -- return n {load = Load {num = i1}, node = t}

-- IDK Object with attributes is a term
-- application with attributes is a term


getId :: Node a Load -> Int
getId Node{..} =
  case load of
    Load i -> i


-- | annotate a node with term id and CST node id
dec :: I a -> State MyState b -> State MyState (K b)
dec n t = do
  t1 <- t
  MyState tId <- get
  put (MyState (tId + 1))
  return Ann {term = t1, ann = IDs {runtimeId = Just tId, treeId = Just (getId n)}}

getTermProgram :: I TProgram -> K Term
getTermProgram p = evalState (toTermProgram p) MyState {termId = 0}

toTermProgram :: I TProgram -> State MyState (K Term)
toTermProgram n@Node {node = TProgram {..}} =
  toTermObjects o


-- toTermMetas :: I TMetas -> State MyState (K TMetas)
-- toTermMetas n@Node {node = TMetas {..}} = dec n $ do
--     cs' <- mapM toTermMeta ms
--     return $ TMetas cs'



-- toTermMeta :: I TMeta -> State MyState (K TMeta)
-- toTermMeta n@Node {node = TMeta {..}} = dec n $ do
--     name' <- toTermName name
--     suff' <- toTermMaybe toTermMetaSuffix suff
--     return $ TMeta name' suff'


-- toTermMetaSuffix :: I TMetaSuffix -> State MyState (K TMetaSuffix)
-- toTermMetaSuffix n@Node {..} = dec n $ return node


toTermName :: I TName -> State MyState (K LetterName)
toTermName m@Node {node = TName t1} = dec m $ return (LetterName t1)

-- should produce an object
toTermObjects :: I TObjects -> State MyState (K Term)
toTermObjects n@Node {node = TObjects {..}} = dec n $ do
  os' <- mapM composeObject os
  return Obj {freeAttrs = [], args = os'}


{-
if there is an object context
attributes that are args to some names should become
args attributes of such object

-- TODO span attributes when processing tail

for cases like

[a]
  b
-}
-- composeAbstractAttribute :: I a -> AttachedOrArg -> [AttachedOrArg] -> State MyState AttachedOrArg
-- composeAbstractAttribute n a t = do
--     -- TODO work with a list of (not optionally) named attributes
--     -- TODO ensure list of attrib
--     -- let f e =
--     --       case e of
--     --         Opt2A p -> Opt2A p
--     --         Opt2B AttachedOrArg {a=a1, t=t1} ->
--     --           case a1 of
--     --             Just a2 ->
--     --               case a2 of
--     --                 Opt2A _ -> e
--     --                 Opt2B _ -> error "abstraction cannot have attribute names of the form a:a"
--     --             Nothing ->
--     --               Opt2A AttachedOrArg {t = t1, a = Nothing}
--     -- TODO check this list doesn't have expressions with HasName
--     -- 0
--     -- let t1 = f <$> t
--     -- IDK do we care about the name?
--     let AttachedOrArg {t = Ann {term  = t2@Obj {..}}} = a
--     t3 <- dec n $ return Obj {freeAttrs = freeAttrs, args = undefined}
--     return AttachedOrArg {t = t3, a = undefined}

{- | for expressions like
x:a
  b
-}
-- composeApplicationAttribute :: I a -> AttachedOrArg -> [AttachedOrArg] -> State MyState AttachedOrArg
-- composeApplicationAttribute n a t = do
--     let AttachedOrArg {a = a1, t = t1} = a
--     t2 <- dec n $ return App {t = t1, args = t}
--     return AttachedOrArg {t = t2, a = a1}


-- applyTail :: I a -> AttachedOrArg -> [AttachedOrArg] -> State MyState AttachedOrArg
-- applyTail n AttachedOrArg{..} as = do
--   let a1@Ann {term = t1} = t
--   let t2 =
--         case t1 of
--           p@Obj {..} -> p {args = as}
--           p@App {..} -> p {args = as}
--           _ -> t1
--   return undefined

{- | produces weird things

for now, it produces either an object or application, depending on context

named object: if
  [a] > b
    c

named application:
  if

    a > b
      c

  or

    a:b
      c

-- TODO also handle cases like

  a > b
    .c
  .d > e
-}

composeObject :: I TObject -> State MyState AttachedOrArg
composeObject n@Node {node = TObject {..}} = do
  -- TODO pass this tail into abstraction or application
  t' <- maybe (return []) toTermTail t

  let applyTail p1@AttachedOrArg {t = p2@Ann {term = t1}} =
        (\x -> p1 {t = p2 {term = x}}::AttachedOrArg) $
        case t1 of
          App {args = as} -> t1 {args = as <> t'}
          Obj {args = as} -> t1 {args = as <> t'}
          _               -> t1

  a' <- applyTail <$>
    case a of
      Opt2A p -> composeAbstraction p
      Opt2B p -> composeApplication p
  -- expressions after EOL
  -- at <- applyTail n a' t'
  -- TODO
  return a'
    -- case a' of
    --   Opt2A b -> Opt2A <$> composeAbstractAttribute n b t'
    --   Opt2B b -> Opt2B <$> composeApplicationAttribute n b t'

    -- TODO correctly handle such tail, not ignore it
    -- let
    --     g (m, h, suff, t1) =
    --         do
    --             m1 <- composeMethod m
    --             h1 <- toTermMaybe composeHtail h
    -- compose         s1 <- toTermMaybe toTermSuffix suff
    --             t2 <- toTermMaybe toTermTail t1
    --             return (m1,h1,s1,t2)
    -- s' <- mapM g s


{-
-- TODO

Anywhere a new name shows up after the > symbol,
it is a declaration of a new attribute in the nearest object abstraction.
-}

{-
list of free attributes and the name of abstraction


can be of form

[a b] (a > b) (c > d)
  e > g
  f > h

-- IDK
is `[a b] (a > b) (c > d)` applied to `e` and `f`
or they become its attributes?

For now, we just append this tail to the inline list and see what happens to object

if there is no abstraction tail, should become a multiline anonymous object

[a b]
  c
  d

-- IDK
[a b] (a > b) (c > d) e (f > g)
how to interpret this
Does it mean that `[a b] (a > b) (c > d)` is applied to `e` and `(f > g)`
-}


composeAbstraction :: I TAbstraction -> State MyState AttachedOrArg
composeAbstraction n@Node {node = TAbstraction {..}} = do
  as' <- composeFreeAttributes as
  t' <-
    case t of
      -- TODO better type
      Just t1 -> Just <$> composeAbstractionTail t1
      Nothing -> return Nothing

  let m name ks = (\t1 -> AttachedOrArg {t = t1, a = name}) <$> dec n (return Obj {freeAttrs = as', args = ks})

  case t' of
    Just p ->
      case p of
        Opt2A a -> m [Opt2A a] []
        Opt2B b -> m [Opt2B Nothing] b
    Nothing ->
      m [Opt2B Nothing] []

-- TODO
toTermTail :: I TTail -> State MyState [AttachedOrArg]
toTermTail Node {node = TTail {..}} = mapM composeObject os

{-
What's the meaning of `(a > a).a`?

Is it the same as
a > a
.a
?

Should we allow it or let the expressions in parentheses only for grouping?
(a b c)
-}



composeApplication :: I TApplication -> State MyState AttachedOrArg
composeApplication n@Node {node = TApplication {..}} = do
  s' <-
    case s of
      -- IDK
      -- we artificially append a head as a method to get a term
      -- but head may contain inappropriate data
      Opt2A a -> (\y -> AttachedOrArg {t = y, a = []}) <$> dec a ((\x -> HeadTerm {n = Nothing, a = Just x}) <$> composeHead a)
      -- application in parentheses
      -- TODO check doesn't need tail arguments
      Opt2B a -> composeApplication a

  -- TODO put arguments from htail inside if s' contains application
  h' <- maybe (return []) composeHtail h
  -- t1 <- dec n $ return App {t = s', args = h'}
  toTermApplication1 (applyTail s' h') a1

{-
-- TODO add a separate type for unnamed terms?
We can return just `AttachedOrArg` without a name
instead of a term
-}

toTermApplication1 :: AttachedOrArg -> I TApplication1 -> State MyState AttachedOrArg
toTermApplication1 t@AttachedOrArg{a = a1} Node {node = TApplication1 {..}} =
  case c of
    Just x  -> composeApplication1Elem t x
    Nothing -> return (t {a = a1 <> [Opt2B Nothing]} :: AttachedOrArg)


{-
-- IDK
does `c` in `a b:c` refer to `b` or to `a b`?
Let us make `c` refer to `a b` only
To make it refer to `b` only, we can write `a (b:c)`
-}

{-
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

-}

initAnn x = Ann {term = x, ann = IDs {treeId = Nothing, runtimeId = Nothing}}

-- | apply term to a list of arguments
applyTail :: AttachedOrArg -> [AttachedOrArg] -> AttachedOrArg
applyTail t@AttachedOrArg{t = t1@Ann {term = t2}} ts = 
  (\z -> t {t = t1 {term = z}} :: AttachedOrArg) $
    case t2 of
      App x y -> App {t = x, args = y <> ts}
      Obj x y -> Obj {freeAttrs = x, args = y <> ts}
      Dot _ _ -> App {t = AttachedOrArg { t = initAnn t2, a = []}, args = ts}
      HeadTerm _ _ -> App {t = AttachedOrArg { t = initAnn t2, a = []}, args = ts}
        

composeApplication1Elem :: AttachedOrArg -> I TApplication1Elem -> State MyState AttachedOrArg
composeApplication1Elem t@AttachedOrArg{a = a1} n@Node {node = TApplication1Elem {..}} = do
  c1' <-
    case c1 of
      -- append method name to an application
      Opt3A b -> 
        (\x -> AttachedOrArg {a = [Opt2B Nothing], t = x}) <$> 
        dec b
          ((\y -> Dot {t = t, attr = y}) <$> composeMethod b)
      Opt3B b -> toTermHas t b
      -- TODO put tail into term
      -- TODO reuse t
      Opt3C b -> (\x -> t {a = a1 <> [Opt2A AttachedName {a = x, imported = Nothing}]} :: AttachedOrArg) <$> composeSuffix b

  -- TODO during parsing, allow
  -- ((method | has) htail? application1) | suffix
  -- IDK
  -- what means method after suffix
  -- or
  -- as well as has after suffix
  -- or arguments
  -- third:foo > x...!
  -- for now, suppose we can return on suffix
  --

  -- TODO add htail inside application
  ht' <- maybe (return []) composeHtail ht


  -- when ((case c1 of Opt3C _ -> True; _ -> False) && not (null ht')) (error "htail after suffix")

  -- TODO report error instead of taking the term and passing it
  -- case c1 of
  --   Opt3B _ -> {- error "Nonsense application of form a:b c" -} guard True
  --   Opt3C _ ->
  --     case ht' of
  --       [] -> guard True
  --       _ -> {- error "Nonsense application of form a > b c" -} guard True
  --   _ -> guard True

  -- previous checks should not allow the application to be named,
  -- so we don't care about the name from c1'
  -- at <- dec n $ return App {t = c1', args = ht'}
  toTermApplication1 (applyTail c1' ht') a


composeMethod :: I TMethod -> State MyState (K MethodName)
composeMethod n@Node {node = TMethod {..}} = dec n $ do
  let m' =
        case m of
          Opt2A Node{node=TName t1} -> MName t1
          Opt2B Node{node=t1} ->
            case t1 of
              MethodRho    -> MRho
              MethodVertex -> MVertex
              MethodAt     -> MAt
  return m'


toTermHas :: AttachedOrArg -> I THas -> State MyState AttachedOrArg
toTermHas t@AttachedOrArg{a = a1} m@Node {node = THas {..}} = do
  let Node {node = TName t1} = n
  h <- dec m $ return (HName t1)
  return (t {a = a1 <> [Opt2B (Just h)]} :: AttachedOrArg)


composeFreeAttributes :: I TAttributes -> State MyState [K Label]
composeFreeAttributes Node {node = TAttributes {..}} =
  mapM composeFreeAttribute as


composeFreeAttribute :: I TFreeAttribute -> State MyState (K Label)
composeFreeAttribute n@Node {node = TFreeAttribute {..}} = dec n $ do
  let l' =
        case l of
          Opt3A Node{node = LabelAt}    -> LAt
          Opt3B Node{node = TName t1}   -> LName t1
          Opt3C Node{node = TVarArg t1} -> LVarArg t1
  return l'


composeAbstractionTail :: I TAbstractionTail -> State MyState AbstractionTail
composeAbstractionTail Node {node = TAbstractionTail {..}} =
  case e of
    Opt2A (a,b) -> do
      a1 <- composeSuffix a
      b1 <- (
        case b of
          Just b' -> Just <$>
            case b' of
              Opt2A c -> Opt2A <$> toTermName c
              Opt2B c -> Opt2B <$> composeTerminal c
          Nothing -> return Nothing
        )
      return (Opt2A AttachedName {a = a1, imported = b1})
    Opt2B h -> Opt2B <$> composeHtail h

-- initLocator :: K Term
-- initLocator = Ann {term = HeadTerm {n = Nothing, a = Nothing}, ann = IDs {treeId = Nothing, runtimeId = Nothing}}


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
composeHtail :: I THtail -> State MyState [AttachedOrArg]
composeHtail Node {node = THtail {..}} = do
    let f e =
            case e of
              -- Return an application attribute. We can always extract a term from it
              Opt3A a -> (\y -> AttachedOrArg {t = y, a = [Opt2B Nothing]}) <$> dec a ((\x -> HeadTerm {n = Nothing, a = Just x}) <$> composeHead a)
              -- it's an application in parentheses
              Opt3B a -> composeApplication a
              -- TODO add case for explicit construction of Opts
              Opt3C a -> composeAbstraction a
    mapM f t

{-
-- TODO 
[]
  Q.x.f.d Q Q
  &.@.< > t
  ^.@.hey > you...
varargs can be in label

-}

composeLabel :: I TLabel -> State MyState (K Label)
composeLabel n@Node {node = TLabel {..}} = dec n $ do
    let l' =
          case l of
            Opt2A _ -> LAt
            Opt2B (Node{node=TName n1}, t) ->
              case t of
                Just _  -> LVarArg n1
                Nothing -> LName n1
    return l'

composeSuffix :: I TSuffix -> State MyState SuffixName
composeSuffix Node {node = TSuffix {..}} = do
  l' <- composeLabel l
  let c' = Data.Maybe.isJust c
  return $ SuffixName {n = l', isConst = c'}

composeTerminal :: I a -> State MyState (K a)
composeTerminal n@Node {..} = dec n $ return node

composeHead :: I THead -> State MyState (K Head)
composeHead n@Node {node = THead {..}} = dec n $ do
  let d = Data.Maybe.isJust dots
  t' <-
    case t of
      Opt3A a -> Opt3A <$> composeTerminal a
      Opt3B a -> Opt3B <$> composeHeadName a
      Opt3C a -> Opt3C <$> composeData a
  return Head {h = t', unpacked = d}


composeHeadName :: I THeadName -> State MyState (K HeadName)
composeHeadName n@Node {node = THeadName {..}} = dec n $ do
  let Node {node = TName t} = name
  hn <- dec name $ return (LetterName t)
  let c' =
        case c of
          Just Node {node = n1} ->
            case n1 of
              PEO.HeadDot  -> Just MInverseDot
              PEO.HeadCopy -> Just MCopy
          Nothing -> Nothing
  return HeadName {n = hn, m = c'}


composeData :: I TData -> State MyState (K DataValue)
composeData n@Node {node = TData {..}} = dec n $
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

composeBool :: I TBool -> State MyState DataValue
composeBool Node {..} = do
  let TBool i = node
  return $ DBool i


composeText :: I TText -> State MyState DataValue
composeText Node {..} = do
  let TText i = node
  return $ DText i


composeHex :: I THex -> State MyState DataValue
composeHex Node {..} = do
  let THex i = node
  return $ DHex i

composeString :: I TString -> State MyState DataValue
composeString Node {..} = do
  let TString i = node
  return $ DString i

composeFloat :: I TFloat -> State MyState DataValue
composeFloat Node {..} = do
  let TFloat i = node
  return $ DFloat i

composeInt :: I TInt -> State MyState DataValue
composeInt Node {..} = do
  let TInt i = node
  return $ DInt i

composeBytes :: I TBytes -> State MyState DataValue
composeBytes Node {node = TBytes {..}} =
    DBytes <$>
  case bs of
      Opt2A t -> Opt2A <$> composeByte t
      Opt2B t -> Opt2B <$> mapM composeLineBytes t

composeChar :: I TChar -> State MyState DataValue
composeChar Node{..} = do
  let TChar i = node
  return $ DChar i

composeRegexBody :: I TRegexBody -> State MyState (K DRegexBody)
composeRegexBody n@Node {..} = dec n $ do
  let TRegexBody {..} = node
  return $ DRegexBody b

composeRegexSuffix :: I TRegexSuffix -> State MyState (K DRegexSuffix)
composeRegexSuffix n@Node {..} = dec n $ do
  let TRegexSuffix {..} = node
  return $ DRegexSuffix s

composeRegex :: I TRegex -> State MyState DataValue
composeRegex Node{..} = do
  let TRegex {..} = node
  r' <- composeRegexBody r
  s' <- composeRegexSuffix suff
  return (DRegex r' s')

composeLineBytes :: I TLineBytes -> State MyState (K DLineBytes)
composeLineBytes n@Node {node = TLineBytes {..}} = dec n $ do
  bs' <- mapM composeByte bs
  return (DLineBytes bs')


composeByte :: Node TByte Load -> State MyState (K DByte)
composeByte n@Node{..} = dec n $ do
  let TByte b = node
  return (DByte b)
