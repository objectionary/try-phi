{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module ToTerm(toTermProgram, getTermProgram, Term(..)) where


import           Control.Monad.State        (get, put)
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
type K a = Ann a Annotation


-- Attribute names

-- if a constructor includes composite types, these types should be declared separately

data Label =
      FName Text
    | FAt
    | FVarArg Text
    deriving (Eq, Ord, Generic, Show, Hashable)


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

-- Attached
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

data DataValue
  = DBool (K Bool)
  | DBytes (Options2  (K DByte) [K DLineBytes])
  | DChar (K Char)
  | DFloat (K Scientific)
  | DHex (K Integer)
  | DInt (K Integer)
  | DRegex (K Text) (K Text)
  | DString (K Text)
  | DText (K Text)
 deriving (Show)

-- TODO define when to throw exceptions
-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

-- data AbstrName = AbstrName {a::SuffixName, imported::Maybe (K Text)}
-- data AbstractionNamed = AbstractionNamed {a::Maybe AbstrName, t::K Term}

data AttachedName = AttachedName {a::SuffixName, imported::Maybe (Options2 (K LetterName) (K TAbstrQuestion))}  deriving (Show)

-- In Object After abstraction, we expect that all attributes in tail have some form of suffix name
-- they can be appnamed, but shouldn't be constructed with an optional argument name
-- application name is in the subset of abstraction names, just doesn't have imported part
data Attached = Attached {t::K Term, a::Maybe AttachedName}  deriving (Show)

-- TODO it's application attribute's name. It cannot be head like data
-- data NamedApp = NamedApp {name::Maybe (K Head), t::K Term}

-- TODO type
-- runtime ids needed for each object
-- except for locators

-- term ids may be present in each term except for locators

data HasName = HName Text | HAt  deriving (Show)

-- in Object after application, we expect that neither attribute has abstraction name (with imported part)
-- so each attribute in tail should be only appnamed
type ArgName = Options2 SuffixName (K HasName)
data AppNamed = AppNamed {a :: Maybe ArgName, t::K Term}  deriving (Show)

data Term
  = App {t::K Term, apps::[AbstrOrApp]}
  | Obj {freeAttrs::[K Label], attached::[Attached]}
  | Dot {t::K Term, attr::K MethodName}
  | Locator {n::Int}
  | HeadTerm {v::K Head}
  | AppNamedTerm AppNamed
  deriving (Show)

-- dec::I a -> (I a -> MapElement) -> ReturnValue
-- dec n m p = do
--     t <- p
    -- MyState {i=i1, m=m1} <- get
    -- put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    -- return n {load = Load {num = i1}, node = t}

-- IDK Object with attributes is a term
-- application with attributes is a term

-- dec = id
-- toTermInsertProgram :: I TProgram -> (I TProgram, MyState)
-- toTermInsertProgram t = runState (toTermProgram t) (MyState {i=0, m=M.empty})

data ReturnValue = ReturnValue {t::K Term}  deriving (Show)

getId :: Node a Load -> Int
getId Node{..} =
  case load of
    Load i -> i
    _      -> error "wrong load"

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
toTermProgram n@Node {node = TProgram {..}} = do
    -- l' <- toTermMaybe toTermLicense l
    -- m' <- toTermMaybe toTermMetas m
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
toTermObjects n@Node {node = TObjects {..}} = do
    os' <- mapM toTermObject os
    let q = Abstraction {attrs = [], name = Nothing}
    Attached {t=t1} <- composeObject n q os'
    return t1

-- TODO use better naming
type AbstrOrApp = Options2 Attached AppNamed

-- if there is an object context
-- attributes that are attached to some names should become
-- attached attributes of such object
composeObject :: I a -> Abstraction -> [AbstrOrApp] -> State MyState Attached
composeObject n a t = do
    let makeAttached e =
          case e of
            Opt2A p -> p
            Opt2B AppNamed {a=a1, t=t1} ->
              case a1 of
                Just a2 ->
                  case a2 of
                    Opt2A a3 -> Attached {t = t1, a = Just AttachedName {a = a3, imported = Nothing}}
                    Opt2B _ -> error "no attribute name for this abstraction attribute"
                Nothing ->
                  Attached {t = t1, a = Nothing}
    let t1 = makeAttached <$> t
    let Abstraction {..} = a
    t2 <- dec n $ return Obj {freeAttrs = attrs, attached = t1}
    return Attached {t = t2, a = name}

{- | for expressions like 
x:a
  b
-}
composeApp :: I TObject -> AppNamed -> [AbstrOrApp] -> State MyState AppNamed
composeApp n a t = do
    let AppNamed {a=a1, t=t1} = a
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
toTermObject :: I TObject -> State MyState AbstrOrApp
toTermObject n@Node {node = TObject {..}} = do
    a' <-
        case a of
            Opt2A p -> Opt2A <$> toTermAbstraction p
            Opt2B p -> Opt2B <$> toTermApplication p
    t' <- maybe (return []) toTermTail t
    case a' of
      Opt2A b -> Opt2A <$> composeObject n b t'
      Opt2B b -> Opt2B <$> composeApp n b t'

    -- Suppose there is no this object's tail

    -- let
    --     g (m, h, suff, t1) =
    --         do
    --             m1 <- toTermMethod m
    --             h1 <- toTermMaybe toTermHtail h
    --             s1 <- toTermMaybe toTermSuffix suff
    --             t2 <- toTermMaybe toTermTail t1
    --             return (m1,h1,s1,t2)
    -- s' <- mapM g s


-- 
data Abstraction = Abstraction {attrs :: [K Label], name::Maybe AttachedName}  deriving (Show)
toTermAbstraction :: I TAbstraction -> State MyState Abstraction
toTermAbstraction Node {node = TAbstraction {..}} = do
    as' <- toTermAttributes as
    t' <- 
        case t of
          Just t1 -> Just <$> toTermAbstractionTail t1
          Nothing -> return Nothing
    return Abstraction {attrs = as', name = t'}


toTermTail :: I TTail -> State MyState [AbstrOrApp]
toTermTail n@Node {node = TTail {..}} = return [] {-dec n $ do
    os' <- mapM toTermObject os
    return $ TTail os'-}

-- toTermMaybe :: Monad f => (a -> f a) -> Maybe a -> f (Maybe a)
-- toTermMaybe :: Monad f => (a -> f b) -> Maybe a -> f (Maybe b)
-- toTermMaybe f x =
--     case x of
--         Just x' -> Just <$> f x'
--         Nothing -> return Nothing  

-- TODO
-- head with arguments becomes a term
-- should head become a term first?
-- application with arguments becomes a term

-- application can have a suffix
toTermApplication :: I TApplication -> State MyState AppNamed
toTermApplication n@Node {node = TApplication {..}} = do
  let filterSuffix AppNamed {..} =
        case a of
          Just _ -> error "this application shouldn't have a suffix"
          Nothing -> t
  s' <-
      case s of
          Opt2A a -> dec a $ HeadTerm <$> toTermHead a
          Opt2B a -> filterSuffix <$> toTermApplication a
  h' <- maybe (return []) toTermHtail h
  let h1 = Opt2B <$> h'
  t1 <- dec n $ return App {t = s', apps = h1}
  toTermApplication1 t1 a1


toTermApplication1 :: K Term -> I TApplication1 -> State MyState AppNamed
toTermApplication1 t Node {node = TApplication1 {..}} =  do
  case c of
      Just x  -> toTermApplication1Elem t x
      Nothing -> return AppNamed {a = Nothing, t = t}


-- TODO fix annotations
toTermApplication1Elem :: K Term -> I TApplication1Elem -> State MyState AppNamed
toTermApplication1Elem t n@Node {node = TApplication1Elem {..}} = do
    c1' <-
        case c1 of
            Opt3A b -> AppNamed Nothing <$> dec b (ToTerm.Dot t <$> toTermMethod b)
            Opt3B b -> toTermHas t b
            Opt3C b -> flip AppNamed t <$> Just <$> Opt2A <$> toTermSuffix b
    let
        c2::K Term
        c2 = Ann {term = AppNamedTerm c1', ann = IDs Nothing Nothing}
    ht' <- maybe (return []) toTermHtail ht
    let h1 = Opt2B <$> ht'
    let a1 = Ann {term = App {t = c2, apps = h1}, ann = IDs Nothing Nothing}
    toTermApplication1 a1 a


toTermMethod :: I TMethod -> State MyState (K MethodName)
toTermMethod n@Node {node = TMethod {..}} = dec n $ do
    let m' =
          case m of
            Opt2A Node{node=TName t1} -> MName t1
            Opt2B Node{node=t1} ->
              case t1 of
                MethodRho -> MRho
                MethodVertex -> MVertex
                MethodAt -> MAt
                _ -> error "wrong terminal as method name"
    return m'


toTermHas :: K Term -> I THas -> State MyState AppNamed
toTermHas t m@Node {node = THas {..}} = do
    let Node {node = TName t1} = n
    h <- dec m $ return (HName t1)
    return $ AppNamed {a = Just (Opt2B h), t = t}


toTermAttributes :: I TAttributes -> State MyState [K Label]
toTermAttributes Node {node = TAttributes {..}} = do
    mapM toTermFreeAttribute as


toTermFreeAttribute :: I TFreeAttribute -> State MyState (K Label)
toTermFreeAttribute n@Node {node = TFreeAttribute {..}} = dec n $ do
    let l' =
          case l of
            Opt3A Node{..} ->
                  case node of
                    LabelAt -> FAt
            Opt3B Node{node = TName t1} -> FName t1
            Opt3C Node{node = TVarArg t1} -> FVarArg t1
    return l'


toTermAbstractionTail :: I TAbstractionTail -> State MyState AttachedName
toTermAbstractionTail Node {node = TAbstractionTail {..}} = do
    case e of
        Opt2A (a,b) -> do
            a1 <- toTermSuffix a
            b1 <- (
                case b of
                    Just b' -> Just <$>
                        case b' of
                            Opt2A c -> Opt2A <$> toTermName c
                            Opt2B c -> Opt2B <$> toTermTerminal c
                    Nothing -> return Nothing
                )
            return AttachedName {a = a1, imported = b1}
        -- TODO correctly process htail
        -- needed for inline anonymous objects
        Opt2B h -> return AttachedName {a = SuffixName {n = Ann {term = FAt, ann = IDs {treeId = Just 1, runtimeId = Just 2}}, isConst = False}, imported = Nothing}


toTermHtail :: I THtail -> State MyState [AppNamed]
toTermHtail Node {node = THtail {..}} = do
    let f e =
            case e of
                Opt3A a -> AppNamed Nothing <$> dec a (HeadTerm <$> toTermHead a)
                -- it's an application in parentheses
                Opt3B a -> toTermApplication a
                -- TODO correctly handle no body abstraction
                Opt3C a -> AppNamed Nothing <$> dec a (return (Locator 3))
                -- Opt3C _ -> error "cannot convert abstraction without body to term"
    mapM f t


toTermLabel :: I TLabel -> State MyState (K Label)
toTermLabel n@Node {node = TLabel {..}} = dec n $ do
    let l' =
          case l of
            Opt2A _ -> FAt
            Opt2B (Node{node=TName n1}, t) ->
                  case t of
                    Just _ -> FVarArg n1
                    Nothing -> FName n1
    return l'


data SuffixName = SuffixName {n::K Label, isConst::Bool}  deriving (Show)
toTermSuffix :: I TSuffix -> State MyState SuffixName
toTermSuffix Node {node = TSuffix {..}} = do
    l' <- toTermLabel l
    let c' = Data.Maybe.isJust c
    return $ SuffixName {n = l', isConst = c'}


toTermTerminal :: I a -> State MyState (K a)
toTermTerminal n@Node {..} = dec n $ return node

-- what is it?
-- ...3
-- ...s'
-- ...s
-- ...s.

toTermHead :: I THead -> State MyState (K Head)
toTermHead n@Node {node = THead {..}} = dec n $ do
    let d = Data.Maybe.isJust dots
    t' <-
        case t of
            Opt3A a -> Opt3A <$> toTermTerminal a
            Opt3B a -> Opt3B <$> toTermHeadName a
            Opt3C a -> Opt3C <$> toTermData a
    return Head {h = t', unpacked = d}


toTermHeadName :: I THeadName -> State MyState (K HeadName)
toTermHeadName n@Node {node = THeadName {..}} = dec n $ do
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


toTermData :: I TData -> State MyState (K DataValue)
toTermData n@Node {node = TData {..}} = dec n $ do
  case d of
      Opt9A a -> toTermBool a
      Opt9B a -> toTermText a
      Opt9C a -> toTermHex a
      Opt9D a -> toTermString a
      Opt9E a -> toTermFloat a
      Opt9F a -> toTermInt a
      Opt9G a -> toTermBytes a
      Opt9H a -> toTermChar a
      Opt9I a -> toTermRegex a

decData :: I a -> b -> State MyState (K b)
decData n i = dec n $ return i

toTermBool :: I TBool -> State MyState DataValue
toTermBool n@Node {..} = do
  let TBool i = node
  DBool <$> decData n i


toTermText :: I TText -> State MyState DataValue
toTermText n@Node {..} = do
  let TText i = node
  DText <$> decData n i


toTermHex :: I THex -> State MyState DataValue
toTermHex n@Node {..} = do
  let THex i = node
  DHex <$> decData n i

toTermString :: I TString -> State MyState DataValue
toTermString n@Node {..} = do
  let TString i = node
  DString <$> decData n i

toTermFloat :: I TFloat -> State MyState DataValue
toTermFloat n@Node {..} = do
  let TFloat i = node
  DFloat <$> decData n i

toTermInt :: I TInt -> State MyState DataValue
toTermInt n@Node {..} = do
  let TInt i = node
  DInt <$> decData n i

toTermBytes :: I TBytes -> State MyState DataValue
toTermBytes n@Node {node = TBytes {..}} = do
    DBytes <$>
      case bs of
          Opt2A t -> Opt2A <$> toTermByte t
          Opt2B t -> Opt2B <$> mapM toTermLineBytes t

toTermChar :: I TChar -> State MyState DataValue
toTermChar n@Node{..} = do
  let TChar c = node
  DChar <$> decData n c


toTermRegex :: I TRegex -> State MyState DataValue
toTermRegex n@Node{..} = do
  let TRegex t1@Node{node=TRegexBody t1'} t2@Node{node = TRegexSuffix t2'} = node
  t3 <- decData t1 t1'
  t4 <- decData t2 t2' 
  return (DRegex t3 t4)


toTermLineBytes :: I TLineBytes -> State MyState (K DLineBytes)
toTermLineBytes n@Node {node = TLineBytes {..}} = dec n $ do
  bs' <- mapM toTermByte bs
  return (DLineBytes bs')


toTermByte :: Node TByte Load -> State MyState (K DByte)
toTermByte n@Node{..} = dec n $ do
  let TByte b = node
  return (DByte b)
