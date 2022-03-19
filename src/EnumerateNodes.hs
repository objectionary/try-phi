{-# LANGUAGE RecordWildCards #-}
module EnumerateNodes where

import           Control.Monad.State.Strict (State, get, put, runState)
import qualified Data.HashMap.Strict.InsOrd as M (InsOrdHashMap, empty, insert)
import           ParseEOAlt

data MapElement =
     MProgram (I TProgram)
    | MLicense (I TLicense)
    | MComment (I TComment)
    | MMetas (I TMetas)
    | MMeta (I TMeta)
    | MMetaSuffix (I TMetaSuffix)
    | MName (I TName)
    | MObjects (I TObjects)
    | MObject (I TObject)
    | MAbstraction (I TAbstraction)
    | MTail (I TTail)
    | MApplication (I TApplication)
    | MApplication1 (I TApplication1)
    | MApplication1Elem (I TApplication1Elem)
    | MMethod (I TMethod)
    | MHas (I THas)
    | MAttributes (I TAttributes)
    | MAbstractionTail (I TAbstractionTail)
    | MHtail (I THtail)
    | MLabel (I TLabel)
    | MSuffix (I TSuffix)
    | MTerminal (I TTerminal)
    | MHead (I THead)
    | MHeadName (I THeadName)
    | MData (I TData)
    | MBool (I TBool)
    | MText (I TText)
    | MHex (I THex)
    | MString (I TString)
    | MFloat (I TFloat)
    | MInt (I TInt)
    | MBytes (I TBytes)
    | MChar (I TChar)
    | MRegex (I TRegex)
    | MLineBytes (I TLineBytes)
    | MByte (I TByte)

data MyState = MyState {i::Int, m::M.InsOrdHashMap Int MapElement}

dec::I a -> (I a -> MapElement) -> State MyState a -> State MyState (I a)
dec n m p = do
    t <- p
    MyState {i=i1, m=m1} <- get
    put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    return n {load = Load {num = i1}, node = t}

enumInsertProgram :: I TProgram -> (I TProgram, MyState)
enumInsertProgram t = runState (enumProgram t) (MyState {i=0, m=M.empty})

enumProgram :: I TProgram -> State MyState (I TProgram)
enumProgram n@Node {node = TProgram {..}} = dec n MProgram $ do
    l' <- enumMaybe enumLicense l
    m' <- enumMaybe enumMetas m
    o' <- enumObjects o
    return $ TProgram l' m' o'


enumLicense :: I TLicense -> State MyState (I TLicense)
enumLicense n@Node {node = TLicense {..}} = dec n MLicense $ do
    cs' <- mapM enumComment cs
    return $ TLicense cs'


enumComment :: I TComment -> State MyState (I TComment)
enumComment n@Node {..} = dec n MComment $ return node


enumMetas :: I TMetas -> State MyState (I TMetas)
enumMetas n@Node {node = TMetas {..}} = dec n MMetas $ do
    cs' <- mapM enumMeta ms
    return $ TMetas cs'


enumMeta :: I TMeta -> State MyState (I TMeta)
enumMeta n@Node {node = TMeta {..}} = dec n MMeta $ do
    name' <- enumName name
    suff' <- enumMaybe enumMetaSuffix suff
    return $ TMeta name' suff'


enumMetaSuffix :: I TMetaSuffix -> State MyState (I TMetaSuffix)
enumMetaSuffix n@Node {..} = dec n MMetaSuffix $ return node


enumName :: I TName -> State MyState (I TName)
enumName m@Node {..} = dec m MName $ return node

enumObjects :: I TObjects -> State MyState (I TObjects)
enumObjects n@Node {node = TObjects {..}} = dec n MObjects $ do
    os' <- mapM enumObject os
    return $ TObjects os'

enumObject :: I TObject -> State MyState (I TObject)
enumObject n@Node {node = TObject {..}} = dec n MObject $ do
    cs' <- mapM enumComment cs
    a' <-
        case a of
            Opt2A p -> Opt2A <$> enumAbstraction p
            Opt2B p -> Opt2B <$> enumApplication p
    t' <- enumMaybe enumTail t
    -- TODO
    let
        g (m,h,suff,t1) =
            do
                m1 <- enumMethod m
                h1 <- enumMaybe enumHtail h
                s1 <- enumMaybe enumSuffix suff
                t2 <- enumMaybe enumTail t1
                return (m1,h1,s1,t2)
    s' <- mapM g s
    return $ TObject cs' a' t' s'

enumAbstraction :: I TAbstraction -> State MyState (I TAbstraction)
enumAbstraction n@Node {node = TAbstraction {..}} = dec n MAbstraction $ do
    as' <- enumAttributes as
    t' <- enumMaybe enumAbstractionTail t
    return $ TAbstraction as' t'

enumTail :: I TTail -> State MyState (I TTail)
enumTail n@Node {node = TTail {..}} = dec n MTail $ do
    os' <- mapM enumObject os
    return $ TTail os'

enumMaybe :: Monad f => (a -> f a) -> Maybe a -> f (Maybe a)
enumMaybe f x =
    case x of
        Just x' -> Just <$> f x'
        Nothing -> return x

enumApplication :: I TApplication -> State MyState (I TApplication)
enumApplication n@Node {node = TApplication {..}} = dec n MApplication $ do
    s' <-
        case s of
            Opt2A a -> Opt2A <$> enumHead a
            Opt2B a -> Opt2B <$> enumApplication a
    h' <- enumMaybe enumHtail h
    a1' <- enumApplication1 a1
    return $ TApplication s' h' a1'


enumApplication1 :: I TApplication1 -> State MyState (I TApplication1)
enumApplication1 n@Node {node = TApplication1 {..}} = dec n MApplication1 $ do
    c' <-
        case c of
            Just x  -> Just <$> enumApplication1Elem x
            Nothing -> return c
    return $ TApplication1 c'


enumApplication1Elem :: I TApplication1Elem -> State MyState (I TApplication1Elem)
enumApplication1Elem n@Node {node = TApplication1Elem {..}} = dec n MApplication1Elem $ do
    c1' <-
        case c1 of
            Opt3A t -> Opt3A <$> enumMethod t
            Opt3B t -> Opt3B <$> enumHas t
            Opt3C t -> Opt3C <$> enumSuffix t
    ht' <- enumMaybe enumHtail ht
    a' <- enumApplication1 a
    return $ TApplication1Elem c1' ht' a'


enumMethod :: I TMethod -> State MyState (I TMethod)
enumMethod n@Node {node = TMethod {..}} = dec n MMethod $ do
    m' <- case m of
        Opt2A t -> Opt2A <$> enumName t
        Opt2B t -> Opt2B <$> enumTerminal t
    return $ TMethod m'


enumHas :: I THas -> State MyState (I THas)
enumHas m@Node {node = THas {..}} = dec m MHas $ do
    n' <- enumName n
    return $ THas n'


enumAttributes :: I TAttributes -> State MyState (I TAttributes)
enumAttributes n@Node {node = TAttributes {..}} = dec n MAttributes $ do
    as' <- mapM enumLabel as
    return $ TAttributes as'

enumAbstractionTail :: I TAbstractionTail -> State MyState (I TAbstractionTail)
enumAbstractionTail n@Node {node = TAbstractionTail {..}} = dec n MAbstractionTail $ do
    e' <-
        case e of
            Opt2A (a,b) -> Opt2A <$> do
                a1 <- enumSuffix a
                b1 <- (
                    case b of
                        Just b' -> Just <$>
                            case b' of
                                Opt2A name -> Opt2A <$> enumName name
                                Opt2B t    -> Opt2B <$> enumTerminal t
                        Nothing -> return b
                    )
                return (a1,b1)
            Opt2B h -> Opt2B <$> enumHtail h
    return $ TAbstractionTail e'


enumHtail :: I THtail -> State MyState (I THtail)
enumHtail n@Node {node = THtail {..}} = dec n MHtail $ do
    let f e =
            case e of
                Opt3A h -> Opt3A <$> enumHead h
                Opt3B a -> Opt3B <$> enumApplication a
                Opt3C a -> Opt3C <$> enumAbstraction a
    t' <- mapM f t
    return $ THtail t'


enumLabel :: I TLabel -> State MyState (I TLabel)
enumLabel n@Node {node = TLabel {..}} = dec n MLabel $ do
    l' <-
        case l of
            Opt2A t -> Opt2A <$> enumTerminal t
            -- TODO
            Opt2B (n1, t) ->
                do
                    n1' <- enumName n1
                    t' <- enumMaybe enumTerminal t
                    return $ Opt2B (n1', t')
    return $ TLabel l'


enumSuffix :: I TSuffix -> State MyState (I TSuffix)
enumSuffix n@Node {node = TSuffix {..}} = dec n MSuffix $ do
    l' <- enumLabel l
    c' <- enumMaybe enumTerminal c
    return $ TSuffix l' c'


-- TODO
enumTerminal :: I TTerminal -> State MyState (I TTerminal)
enumTerminal n@Node {..} = dec n MTerminal $ return node


enumHead :: I THead -> State MyState (I THead)
enumHead n@Node {node = THead {..}} = dec n MHead $ do
    dots' <- enumMaybe enumTerminal dots
    t' <-
        case t of
            Opt3A a -> Opt3A <$> enumTerminal a
            Opt3B a -> Opt3B <$> enumHeadName a
            Opt3C a -> Opt3C <$> enumData a
    return $ THead dots' t'


enumHeadName :: I THeadName -> State MyState (I THeadName)
enumHeadName n@Node {node = THeadName {..}} = dec n MHeadName $ do
    name' <- enumName name
    c' <-
        case c of
            Opt2A t -> Opt2A <$> enumTerminal t
            Opt2B t -> Opt2B <$> enumMaybe enumTerminal t
    return $ THeadName name' c'


enumData :: I TData -> State MyState (I TData)
enumData n@Node {node = TData {..}} = dec n MData $ do
    d' <-
        case d of
            Opt9A a -> Opt9A <$> enumBool a
            Opt9B a -> Opt9B <$> enumText a
            Opt9C a -> Opt9C <$> enumHex a
            Opt9D a -> Opt9D <$> enumString a
            Opt9E a -> Opt9E <$> enumFloat a
            Opt9F a -> Opt9F <$> enumInt a
            Opt9G a -> Opt9G <$> enumBytes a
            Opt9H a -> Opt9H <$> enumChar1 a
            Opt9I a -> Opt9I <$> enumRegex a
    return $ TData d'


enumBool :: I TBool -> State MyState (I TBool)
enumBool n@Node {..} = dec n MBool $ return node


enumText :: I TText -> State MyState (I TText)
enumText n@Node {..} = dec n MText $ return node


enumHex :: I THex -> State MyState (I THex)
enumHex n@Node {..} = dec n MHex $ return node


enumString :: I TString -> State MyState (I TString)
enumString n@Node {..} = dec n MString $ return node


enumFloat :: I TFloat -> State MyState (I TFloat)
enumFloat n@Node {..} = dec n MFloat $ return  node


enumInt :: I TInt -> State MyState (I TInt)
enumInt n@Node {..} = dec n MInt $ return node


enumBytes :: I TBytes -> State MyState (I TBytes)
enumBytes n@Node {node = TBytes {..}} = dec n MBytes $ do
    bs' <-
            case bs of
                Opt3A t -> Opt3A <$> enumTerminal t
                Opt3B t -> Opt3B <$> enumByte t
                Opt3C t -> Opt3C <$> mapM enumLineBytes t
    return $ TBytes bs'


enumChar1 :: I TChar -> State MyState (I TChar)
enumChar1 n = dec n MChar $ return $ node n


enumRegex :: I TRegex -> State MyState (I TRegex)
enumRegex n = dec n MRegex $ return $ node n



enumLineBytes :: I TLineBytes -> State MyState (I TLineBytes)
enumLineBytes n@Node {node = TLineBytes {..}} = dec n MLineBytes $ do
    bs' <- mapM enumByte bs
    return $ TLineBytes bs'



enumByte :: I TByte -> State MyState (I TByte)
enumByte n = dec n MByte $ return $ node n
