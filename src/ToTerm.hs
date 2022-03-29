{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}

{-# LANGUAGE DuplicateRecordFields      #-}

{-# LANGUAGE RecordWildCards            #-}

module ToTerm(toTermProgram, getTermProgram, Term(..)) where


import           Control.Monad.State        (get, put, guard)
import           Control.Monad.State.Strict (State, evalState)
import           Data.Hashable              (Hashable)
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           ParseEO                    as PEO
import qualified Data.Maybe
import PrettyPrintTree


type Id = Int
data Ann a b = Ann {term::a, ann::b} deriving (Show)
data Annotation = IDs {treeId::Maybe Id, runtimeId::Maybe Id}  deriving (Show)
-- TODO make annotation optional
type K a = Ann a Annotation


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

-- AbstractNamed
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
  | DText Text
 deriving (Show)

-- TODO define when to throw exceptions
-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

data AbstractName = AbstractName {a::SuffixName, imported::Maybe (Options2 (K LetterName) (K TAbstrQuestion))}  deriving (Show)
data HasName = HName Text | HAt  deriving (Show)

{-
When an object term is ready, it becomes AbstractNamed
-}
data AbstractNamed = AbstractNamed {t::K Term, a::Maybe AbstractName}  deriving (Show)

{-
Not yet a full-fledged abstract attribute
Might not have a name
or attributes that will come later in the next lines

-- TODO better type

-- IDK
should it be different from AbstractNamed?
If abstractionTail has attributes in parentheses, we can construct an AbstractNamed (inline object)
Probably, such object is always anonymous

otherwise, Abstraction should store AbstractName
-}
data Abstraction = Abstraction {attrs :: [K Label], t::Maybe AbstractionTail}  deriving (Show)

type AbstractOrApp = Options2 AbstractNamed AppNamed

{-
For cases like 
[a] (3 > b) c
here, the anonymous abstract object is applied to c
-}
type AbstractionTail = Options2 AbstractName [AbstractOrApp]

{-
In context of application tail,
attributes can have
  no name
  `has` name
  `suffix` name
  `attached` name

So, application arguments should have `Maybe (Options2 AbstractName (K HasName))` names
However, application expressions cannot give `AbstractName`, 
so we use just `AppName`
-}
type AppName = Options2 SuffixName (K HasName)

{-
when an application term is ready, it becomes AppNamed
-}
data AppNamed = AppNamed {t::K Term, a :: Maybe AppName}  deriving (Show)

data Term
  = App {t::K Term, apps::[AbstractOrApp]}
  | Obj {freeAttrs::[K Label], attached::[AbstractOrApp]}
  | Dot {t::K Term, attr::Options2 (K MethodName) (K Head)}
  | Locator {n::Maybe Int}
  deriving (Show)

-- dec::I a -> (I a -> MapElement) -> ReturnValue
-- dec n m p = do
--     t <- p
    -- MyState {i=i1, m=m1} <- get
    -- put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    -- return n {load = Load {num = i1}, node = t}

-- IDK Object with attributes is a term
-- application with attributes is a term

data ReturnValue = ReturnValue {t::K Term}  deriving (Show)

getId :: Node a Load -> Int
getId Node{..} =
  case load of
    Load i -> i

data MyState = MyState {termId :: Int}  deriving (Show)

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
    return Obj {freeAttrs = [], attached = os'}


{-
if there is an object context
attributes that are attached to some names should become
attached attributes of such object
-}
composeAbstractAttribute :: I a -> AbstractNamed -> [AbstractOrApp] -> State MyState AbstractNamed
composeAbstractAttribute n a t = do
    let f e =
          case e of
            Opt2A p -> p
            Opt2B AppNamed {a=a1, t=t1} ->
              case a1 of
                Just a2 ->
                  case a2 of
                    Opt2A a3 -> AbstractNamed {t = t1, a = Just AbstractName {a = a3, imported = Nothing}}
                    Opt2B _ -> error "no attribute name for this abstraction attribute"
                Nothing ->
                  AbstractNamed {t = t1, a = Nothing}
    -- TODO fix
    let t1 = f <$> t
    let AbstractNamed {t = Ann {term  = t2@Obj {..}}} = a
    t2 <- dec n $ return Obj {freeAttrs = freeAttrs, attached = undefined}
    return AbstractNamed {t = t2, a = undefined}

{- | for expressions like 
x:a
  b
-}
composeApplicationAttribute :: I TObject -> AppNamed -> [AbstractOrApp] -> State MyState AppNamed
composeApplicationAttribute n a t = do
    -- TODO cast AbstractOrApp to HTail
    let AppNamed {a = a1, t = t1} = a
    t2 <- dec n $ return App {t = t1, apps = t}
    return AppNamed {t = t2, a = a1}


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

composeObject :: I TObject -> State MyState AbstractOrApp
composeObject n@Node {node = TObject {..}} = do
    a' <-
        case a of
            Opt2A p -> Opt2A <$> composeAbstraction p
            Opt2B p -> Opt2B <$> composeApplication p
    -- expressions after EOL
    t' <- maybe (return []) toTermTail t
    case a' of
      Opt2A b -> Opt2A <$> composeAbstractAttribute n b t'
      Opt2B b -> Opt2B <$> composeApplicationAttribute n b t'

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
list of free attributes and the name of abstraction
-}

composeAbstraction :: I TAbstraction -> State MyState AbstractNamed
composeAbstraction n@Node {node = TAbstraction {..}} = do
    as' <- composeFreeAttributes as
    t' <-
        case t of
          -- TODO better type
          Just t1 -> Just <$> composeAbstractionTail t1
          Nothing -> return Nothing
    t1 <- dec n $ return Obj {freeAttrs = as', attached = []}
    -- TODO check that htail is of a specific form like
    -- [a] (a > b) ([] > c) d
    -- or 
    -- [a] d
    -- but not of
    -- [a] d (a > b)
    -- because we cannot extend objects this way, AFAIK
    -- TODO full object construction, including htail elements
    -- in this case, object will be anonymous
    let n1 = 
          case t' of
            Just k -> 
              case k of
                Opt2A l -> Just l
                -- TODO handle htail
                Opt2B _ -> Nothing
            _ -> Nothing

    return AbstractNamed {t = t1, a = n1}


-- TODO
toTermTail :: I TTail -> State MyState [AbstractOrApp]
toTermTail n@Node {node = TTail {..}} = return [] {-dec n $ do
    composeObject os
    return $ TTail os'-}

{-
What's the meaning of `(a > a).a`?

Is it the same as
a > a
.a
?

Should we allow it or let the expressions in parentheses only for grouping?
(a b c)
-}
composeApplication :: I TApplication -> State MyState AppNamed
composeApplication n@Node {node = TApplication {..}} = do
  s' <-
      case s of
          Opt2A a -> dec a $ (\x -> Dot {t = initLocator, attr = Opt2B x}) <$> composeHead a
          Opt2B a -> (\AppNamed {t = t1} -> t1) <$> composeApplication a
  h' <- maybe (return []) composeHtail h
  t1 <- dec n $ return App {t = s', apps = h'}
  toTermApplication1 t1 a1

{- 
-- TODO add a separate type for unnamed terms?
We can return just `AppNamed` without a name
instead of a term
-}

toTermApplication1 :: K Term -> I TApplication1 -> State MyState AppNamed
toTermApplication1 t Node {node = TApplication1 {..}} =
  case c of
      Just x  -> composeApplication1Elem t x
      Nothing -> return AppNamed {a = Nothing, t = t}


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
    if a isn't applied to some term directly?
      

a > b c
doesn't really make sense
  for the aforementioned reasons

However, if htail is empty in the last expression, e.g.

a > b


This means that we reached the end of the application chain
and can return this named term
-}
composeApplication1Elem :: K Term -> I TApplication1Elem -> State MyState AppNamed
composeApplication1Elem t n@Node {node = TApplication1Elem {..}} = do
    c1' <-
        case c1 of
            -- append method name to an application
            Opt3A b -> (\x -> AppNamed {a = Nothing, t = x}) <$> dec b ((\y -> Dot {t = t, attr = Opt2A y}) <$> composeMethod b)
            Opt3B b -> toTermHas t b
            Opt3C b -> (\x -> AppNamed {t = t, a = Just x}) . Opt2A <$> composeSuffix b

    ht' <- maybe (return []) composeHtail ht

    -- TODO report error instead of taking the term and passing it
    -- case c1 of
    --   Opt3B _ -> {- error "Nonsense application of form a:b c" -} guard True
    --   Opt3C _ ->
    --     case ht' of
    --       [] -> guard True
    --       _ -> {- error "Nonsense application of form a > b c" -} guard True
    --   _ -> guard True

    let AppNamed {t = t'} = c1'

    -- previous checks should not allow the application to be named,
    -- so we don't care about the name from c1'
    at <- dec n $ return App {t = t', apps = ht'}
    toTermApplication1 at a


composeMethod :: I TMethod -> State MyState (K MethodName)
composeMethod n@Node {node = TMethod {..}} = dec n $ do
    let m' =
          case m of
            Opt2A Node{node=TName t1} -> MName t1
            Opt2B Node{node=t1} ->
              case t1 of
                MethodRho -> MRho
                MethodVertex -> MVertex
                MethodAt -> MAt
    return m'


toTermHas :: K Term -> I THas -> State MyState AppNamed
toTermHas t m@Node {node = THas {..}} = do
    let Node {node = TName t1} = n
    h <- dec m $ return (HName t1)
    return $ AppNamed {a = Just (Opt2B h), t = t}


composeFreeAttributes :: I TAttributes -> State MyState [K Label]
composeFreeAttributes Node {node = TAttributes {..}} =
    mapM composeFreeAttribute as


composeFreeAttribute :: I TFreeAttribute -> State MyState (K Label)
composeFreeAttribute n@Node {node = TFreeAttribute {..}} = dec n $ do
    let l' =
          case l of
            Opt3A Node{node = LabelAt} -> LAt
            Opt3B Node{node = TName t1} -> LName t1
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
            return (Opt2A AbstractName {a = a1, imported = b1})
        Opt2B h -> Opt2B <$> composeHtail h

initLocator :: K Term
initLocator = Ann {term = Locator {n = Nothing}, ann = IDs {treeId = Nothing, runtimeId = Nothing}}


{-
head
  is prepended by a locator to produce a term
  need a term to later be accessed by a dot
  assume that data is not directly accessible in the program and is located

-}
composeHtail :: I THtail -> State MyState [AbstractOrApp]
composeHtail Node {node = THtail {..}} = do
    let f e =
            case e of
                -- Return an application attribute. We can always extract a term from it
                Opt3A a -> Opt2B . (\y -> AppNamed {t = y, a = Nothing}) <$> dec a ((\x -> Dot {t = initLocator, attr = Opt2B x}) <$> composeHead a)
                -- it's an application in parentheses
                Opt3B a -> Opt2B <$> composeApplication a
                Opt3C a -> Opt2A <$> composeAbstraction a
    mapM f t

composeLabel :: I TLabel -> State MyState (K Label)
composeLabel n@Node {node = TLabel {..}} = dec n $ do
    let l' =
          case l of
            Opt2A _ -> LAt
            Opt2B (Node{node=TName n1}, t) ->
                  case t of
                    Just _ -> LVarArg n1
                    Nothing -> LName n1
    return l'

data SuffixName = SuffixName {n::K Label, isConst::Bool}  deriving (Show)

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
                PEO.HeadDot -> Just MInverseDot
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
