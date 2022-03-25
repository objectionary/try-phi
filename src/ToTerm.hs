{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module ToTerm where


import           Control.Monad.Identity     (Identity)
import           Control.Monad.State        (get, put)
import           Control.Monad.State.Strict (State)
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Strict.InsOrd as M
import           Data.Scientific            (Scientific)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           ParseEO                    as PEO
import qualified Data.Maybe


type Id = Int
data Ann a b = Ann {term::a, ann::b}
data Annotation = IDs {treeId::Maybe Id, runtimeId::Maybe Id}
type K a = Ann a Annotation


-- Attribute names

-- if a constructor includes composite types, these types should be declared separately

data Label =
      FName Text
    | FAt
    | FVarArg Text
    deriving (Eq, Ord, Generic, Show, Hashable)

type ReservedName = TTerminal

newtype LetterName = LetterName Text
data Modifier = MCopy | MInverseDot
data HeadName = HeadName {n::K LetterName, m::Maybe Modifier}

-- TODO Question is not a terminal

data MethodName =
    MName Text
  | MRoot
  | MAt
  | MVertex
  deriving (Eq, Ord, Generic, Show, Hashable)

-- Attached
-- Unpacked can be ^.x, not necessarily a name

data Head = Head {h::Options3 (K ReservedName) (K HeadName) (K DataValue), unpacked::Bool}

newtype DByte = DByte {byte::Integer}
newtype DLineBytes = DLineBytes {bs :: [K DByte]}

data DataValue
  = DBool Bool
  | DBytes (Options2  (K DByte) [K DLineBytes])
  | DChar Char
  | DFloat Scientific
  | DHex Integer
  | DInt Integer
  | DRegex Text Text
  | DString Text
  | DText Text


-- TODO define when to throw exceptions
-- TODO somehow pass problems with node conversion upwards
-- probably need to use exceptions or Either Id Term

-- data AbstrName = AbstrName {a::SuffixName, imported::Maybe (K Text)}
-- data AbstractionNamed = AbstractionNamed {a::Maybe AbstrName, t::K Term}

data AbstractionName = AbstractionName {a::SuffixName, imported::Maybe (Options2 (K LetterName) (K TTerminal))}
data Attached = Attached {t::K Term, a::Options2 (K AbstractionName) SuffixName}

-- TODO it's application attribute's name. It cannot be head like data
-- data NamedApp = NamedApp {name::Maybe (K Head), t::K Term}

-- TODO type
-- runtime ids needed for each object
-- except for locators

-- term ids may be present in each term except for locators

data HasName = HName Text | HAt

type ArgName = Options2 SuffixName (K HasName)
data AppNamed = AppNamed {a :: Maybe ArgName, t::K Term}

data Term
  = App {t::K Term, apps::[AppNamed]}
  | Obj {freeAttrs::[K Label], obj::M.InsOrdHashMap Label Attached}
  | Dot {t::K Term, attr::K MethodName}
  | Locator {n::Int}
  | HeadTerm {v::K Head}
  | AppNamedTerm AppNamed


-- dec::I a -> (I a -> MapElement) -> ReturnValue
-- dec n m p = do
--     t <- p
    -- MyState {i=i1, m=m1} <- get
    -- put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    -- return n {load = Load {num = i1}, node = t}


-- dec = id
-- toTermInsertProgram :: I TProgram -> (I TProgram, MyState)
-- toTermInsertProgram t = runState (toTermProgram t) (MyState {i=0, m=M.empty})

data ReturnValue = ReturnValue {t::K Term}

getId :: Node a Load -> Int
getId Node{..} =
  case load of
    Load i -> i
    _      -> error "wrong load"

data MyState = MyState {termId :: Int}

-- | annotate a node with term id and CST node id
dec :: I a -> State MyState b -> State MyState (K b)
dec n t = do
  t1 <- t
  MyState tId <- get
  put (MyState (tId + 1))
  return Ann {term = t1, ann = IDs {runtimeId = Just tId, treeId = Just (getId n)}}


-- toTermInsertProgram :: I TProgram -> (I TProgram, MyState)
-- toTermInsertProgram t = runState (toTermProgram t) (MyState {i=0, m=M.empty})

-- toTermProgram :: I TProgram -> State MyState (K TProgram)
-- toTermProgram n@Node {node = TProgram {..}} = dec n $ do
--     l' <- toTermMaybe toTermLicense l
--     m' <- toTermMaybe toTermMetas m
--     o' <- toTermObjects o
--     return $ TProgram l' m' o'


-- toTermLicense :: I TLicense -> State MyState (K TLicense)
-- toTermLicense n@Node {node = TLicense {..}} = dec n $ do
--     cs' <- mapM toTermComment cs
--     return $ TLicense cs'


-- toTermComment :: I TComment -> State MyState (K TComment)
-- toTermComment n@Node {..} = dec n $ return node


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

-- toTermObjects :: I TObjects -> State MyState (K TObjects)
-- toTermObjects n@Node {node = TObjects {..}} = dec n $ do
--     os' <- mapM toTermObject os
--     return $ TObjects os'

-- toTermObject :: I TObject -> State MyState (K TObject)
-- toTermObject n@Node {node = TObject {..}} = dec n $ do
--     cs' <- mapM toTermComment cs
--     a' <-
--         case a of
--             Opt2A p -> Opt2A <$> toTermAbstraction p
--             Opt2B p -> Opt2B <$> toTermApplication p
--     t' <- toTermMaybe toTermTail t
--     -- TODO
--     let
--         g (m,h,suff,t1) =
--             do
--                 m1 <- toTermMethod m
--                 h1 <- toTermMaybe toTermHtail h
--                 s1 <- toTermMaybe toTermSuffix suff
--                 t2 <- toTermMaybe toTermTail t1
--                 return (m1,h1,s1,t2)
--     s' <- mapM g s
--     return $ TObject cs' a' t' s'

data Abstraction = Abstraction {attrs :: [K Label], name::Maybe (K AbstractionName)}

toTermAbstraction :: I TAbstraction -> State MyState Abstraction
toTermAbstraction Node {node = TAbstraction {..}} = do
    as' <- toTermAttributes as
    t' <- 
        case t of 
          Just t1 -> Just <$> toTermAbstractionTail t1
          Nothing -> return Nothing
    return Abstraction {attrs = as', name = t'}

-- toTermTail :: I TTail -> State MyState (K TTail)
-- toTermTail n@Node {node = TTail {..}} = dec n $ do
--     os' <- mapM toTermObject os
--     return $ TTail os'

-- toTermMaybe :: Monad f => (a -> f a) -> Maybe a -> f (Maybe a)
-- toTermMaybe f x =
--     case x of
--         Just x' -> Just <$> f x'
--         Nothing -> return x

-- TODO
-- head with arguments becomes a term
-- should head become a term first?

-- application with arguments becomes a term

toTermApplication :: I TApplication -> State MyState AppNamed
toTermApplication n@Node {node = TApplication {..}} = do
  -- if an application is right 
  let filterSuffix AppNamed {..} =
        case a of
          Just _ -> error "this application shouldn't have a suffix"
          Nothing -> t
  s' <-
      case s of
          Opt2A a -> dec a $ HeadTerm <$> toTermHead a
          Opt2B a -> filterSuffix <$> toTermApplication a
  h' <- maybe (return []) toTermHtail h
  t1 <- dec n $ return App {t = s', apps = h'}
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
    let
        a1::K Term
        a1 = Ann {term = App {t = c2, apps = ht'}, ann = IDs Nothing Nothing}
    toTermApplication1 a1 a


toTermMethod :: I TMethod -> State MyState (K MethodName)
toTermMethod n@Node {node = TMethod {..}} = dec n $ do
    let m' =
          case m of
            Opt2A Node{node=TName t1} -> MName t1
            Opt2B Node{node=t1} ->
              case t1 of
                PEO.Root -> MRoot
                PEO.Vertex -> MVertex
                PEO.At -> MAt
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
                    PEO.At -> FAt
                    _ -> error "wrong terminal in label"
            Opt3B Node{node = TName t1} -> FName t1
            Opt3C Node{node = TVarArg t1} -> FVarArg t1
    return l'


toTermAbstractionTail :: I TAbstractionTail -> State MyState (K AbstractionName)
toTermAbstractionTail n@Node {node = TAbstractionTail {..}} = dec n $ do
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
            return AbstractionName {a = a1, imported = b1}
        Opt2B h -> error "RLY, htail after abstraction?"


toTermHtail :: I THtail -> State MyState [AppNamed]
toTermHtail Node {node = THtail {..}} = do
    let f e =
            case e of
                Opt3A a -> AppNamed Nothing <$> dec a (HeadTerm <$> toTermHead a)
                -- it's an application in parentheses
                Opt3B a -> toTermApplication a
                Opt3C _ -> error "cannot convert abstraction without body to term"
    mapM f t


toTermLabel :: I TLabel -> State MyState (K Label)
toTermLabel n@Node {node = TLabel {..}} = dec n $ do
    let l' =
          case l of
            Opt2A Node{..} ->
                  case node of
                    PEO.At -> FAt
                    _ -> error "wrong terminal in label"
            Opt2B (Node{node=TName n1}, t) ->
                  case t of
                    Just _ -> FVarArg n1
                    Nothing -> FName n1
    return l'


data SuffixName = SuffixName {n::K Label, isConst::Bool}
toTermSuffix :: I TSuffix -> State MyState SuffixName
toTermSuffix n@Node {node = TSuffix {..}} = do
    l' <- toTermLabel l
    let c' = Data.Maybe.isJust c
    let s = SuffixName l' c'
    return s


toTermTerminal :: I TTerminal -> State MyState (K TTerminal)
toTermTerminal n@Node {..} = dec n $ return node

-- what is it?
-- ...3
-- ...s'
-- ...s
-- ...s.


toTermHead :: I THead -> State MyState (K Head)
toTermHead n@Node {node = THead {..}} = dec n $ do
    t' <-
        case t of
            Opt3A a -> Opt3A <$> toTermTerminal a
            Opt3B a -> Opt3B <$> toTermHeadName a
            Opt3C a -> Opt3C <$> toTermData a
    let d = Data.Maybe.isJust dots
    return Head {h = t', unpacked = d}

toTermHeadName :: I THeadName -> State MyState (K HeadName)
toTermHeadName n@Node {node = THeadName {..}} = dec n $ do
  let Node {node = TName t} = name
  hn <- dec name $ return (LetterName t)
  let c' =
        case c of
            Opt2A Node {node = PEO.Dot} -> Just MInverseDot
            Opt2B p ->
              case p of
                Just Node {node = PEO.Copy} -> Just MCopy
                Nothing -> Nothing
                _ -> error "wrong terminal after head name (not a copy)"
            _ -> error "wrong terminal after head name (not a dot)"
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


toTermBool :: I TBool -> State MyState DataValue
toTermBool n@Node {..} = do
  let TBool i = node
  return (DBool i)


toTermText :: I TText -> State MyState DataValue
toTermText n@Node {..} = do
  let TText i = node
  return (DText i)


toTermHex :: I THex -> State MyState DataValue
toTermHex n@Node {..} = do
  let THex i = node
  return (DHex i)


toTermString :: I TString -> State MyState DataValue
toTermString n@Node {..} = do
  let TString i = node
  return (DString i)


toTermFloat :: I TFloat -> State MyState DataValue
toTermFloat n@Node {..} = do
  let TFloat i = node
  return (DFloat i)


toTermInt :: I TInt -> State MyState DataValue
toTermInt n@Node {..} = do
  let TInt i = node
  return (DInt i)


toTermBytes :: I TBytes -> State MyState DataValue
toTermBytes Node {node = TBytes {..}} = do
    bs' <-
            case bs of
                Opt2A t -> Opt2A <$> toTermByte t
                Opt2B t -> Opt2B <$> mapM toTermLineBytes t
    return (DBytes bs')


toTermChar :: I TChar -> State MyState DataValue
toTermChar n@Node{..} = do
  let TChar c = node
  return (DChar c)

toTermRegex :: I TRegex -> State MyState DataValue
toTermRegex n@Node{..} = do
  let TRegex t1 t2 = node
  return (DRegex t1 t2)

toTermLineBytes :: I TLineBytes -> State MyState (K DLineBytes)
toTermLineBytes n@Node {node = TLineBytes {..}} = dec n $ do
  bs' <- mapM toTermByte bs
  return (DLineBytes bs')

toTermByte :: Node TByte Load -> State MyState (K DByte)
toTermByte n@Node{..} = dec n $ do
  let TByte b = node
  return (DByte b)
