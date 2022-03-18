{-# LANGUAGE RecordWildCards #-}
module EnumerateNodes where

import ParseEOAlt
import Control.Monad.State.Strict (State, get, put)

dec::I a -> State Int a -> State Int (I a)
dec n p = do
    a <- get
    put (a + 1)
    t <- p
    return n {load = Load a, node = t}

enumProgram :: I TProgram -> State Int (I TProgram)
enumProgram n@Node {node = TProgram {..}} = dec n $ do
    l' <- enumMaybe enumLicense l
    m' <- enumMaybe enumMetas m
    o' <- enumObjects o
    return $ TProgram l' m' o'


enumLicense :: I TLicense -> State Int (I TLicense)
enumLicense n@Node {node = TLicense {..}} = dec n $ do
    cs' <- mapM enumComment cs
    return $ TLicense cs'


enumComment :: I TComment -> State Int (I TComment)
enumComment n@Node {..} = dec n $ return node


enumMetas :: I TMetas -> State Int (I TMetas)
enumMetas n@Node {node = TMetas {..}} = dec n $ do
    cs' <- mapM enumMeta ms
    return $ TMetas cs'


enumMeta :: I TMeta -> State Int (I TMeta)
enumMeta n@Node {node = TMeta {..}} = dec n $ do
    name' <- enumName name
    suff' <- enumMaybe enumMetaSuffix suff
    return $ TMeta name' suff'


enumMetaSuffix :: I TMetaSuffix -> State Int (I TMetaSuffix)
enumMetaSuffix n@Node {..} = dec n $ return node


enumName :: I TName -> State Int (I TName)
enumName m@Node {..} = dec m $ return node

enumObjects :: I TObjects -> State Int (I TObjects)
enumObjects n@Node {node = TObjects {..}} = dec n $ do
    os' <- mapM enumObject os
    return $ TObjects os'

enumObject :: I TObject -> State Int (I TObject)
enumObject n@Node {node = TObject {..}} = dec n $ do
    cs' <- mapM enumComment cs
    a' <-
        case a of
            Opt2A p -> Opt2A <$> enumAbstraction p
            Opt2B p -> Opt2B <$> enumApplication p
    t' <- enumMaybe enumTail t
    -- TODO
    let g (m,h,suff,t) = do
        m1 <- enumMethod m
        h1 <- enumMaybe enumHtail h
        s1 <- enumMaybe enumSuffix suff
        t1 <- enumMaybe enumTail t
        return (m1,h,s1,t1)
    s' <- mapM g s
    return $ TObject cs' a' t' s'

enumAbstraction :: I TAbstraction -> State Int (I TAbstraction)
enumAbstraction n@Node {node = TAbstraction {..}} = dec n $ do
    as' <- enumAttributes as
    t' <- enumMaybe enumAbstractionTail t
    return $ TAbstraction as' t'

enumTail :: I TTail -> State Int (I TTail)
enumTail n@Node {node = TTail {..}} = dec n $ do
    os' <- mapM enumObject os
    return $ TTail os'

enumMaybe :: Monad f => (a -> f a) -> Maybe a -> f (Maybe a)
enumMaybe f x =
    case x of
        Just x' -> Just <$> f x'
        Nothing -> return x

enumApplication :: I TApplication -> State Int (I TApplication)
enumApplication n@Node {node = TApplication {..}} = dec n $ do
    s' <- 
        case s of
            Opt2A a -> Opt2A <$> enumHead a
            Opt2B a -> Opt2B <$> enumApplication a
    h' <- enumMaybe enumHtail h
    a1' <- enumApplication1 a1
    return $ TApplication s' h' a1'


enumApplication1 :: I TApplication1 -> State Int (I TApplication1)
enumApplication1 n@Node {node = TApplication1 {..}} = dec n $ do
    c' <- 
        case c of
            Just x -> Just <$> enumApplication1Elem x
            Nothing -> return c
    return $ TApplication1 c'


enumApplication1Elem :: I TApplication1Elem -> State Int (I TApplication1Elem)
enumApplication1Elem n@Node {node = TApplication1Elem {..}} = dec n $ do
    c1' <- 
        case c1 of
            Opt3A t -> Opt3A <$> enumMethod t
            Opt3B t -> Opt3B <$> enumHas t
            Opt3C t -> Opt3C <$> enumSuffix t
    ht' <- enumMaybe enumHtail ht
    a' <- enumApplication1 a
    return $ TApplication1Elem c1' ht' a'


enumMethod :: I TMethod -> State Int (I TMethod)
enumMethod n@Node {node = TMethod {..}} = dec n $ do
    m' <- case m of
        Opt2A t -> Opt2A <$> enumName t
        Opt2B t -> Opt2B <$> enumTerminal t
    return $ TMethod m'


enumHas :: I THas -> State Int (I THas)
enumHas m@Node {node = THas {..}} = dec m $ do 
    n' <- enumName n 
    return $ THas n'


enumAttributes :: I TAttributes -> State Int (I TAttributes)
enumAttributes n@Node {node = TAttributes {..}} = dec n $ do
    as' <- mapM enumLabel as
    return $ TAttributes as'

enumAbstractionTail :: I TAbstractionTail -> State Int (I TAbstractionTail)
enumAbstractionTail n@Node {node = TAbstractionTail {..}} = dec n $ do
    e' <-
        case e of
            Opt2A (a,b) -> Opt2A <$> do
                a1 <- enumSuffix a
                b1 <- (
                    case b of
                        Just b' ->
                            case b' of
                                Opt2A name -> Opt2A <$> enumName name
                                Opt2B t -> Opt2B <$> enumTerminal t
                        -- TODO
                        Nothing -> undefined
                    )
                return (a,b)
            Opt2B h -> Opt2B <$> enumHtail h
    return $ TAbstractionTail e'


enumHtail :: I THtail -> State Int (I THtail)
enumHtail n@Node {node = THtail {..}} = dec n $ do
    let f e =
            case e of
                Opt3A h -> Opt3A <$> enumHead h
                Opt3B a -> Opt3B <$> enumApplication a
                Opt3C a -> Opt3C <$> enumAbstraction a
    t' <- mapM f t
    return $ THtail t'


enumLabel :: I TLabel -> State Int (I TLabel)
enumLabel n@Node {node = TLabel {..}} = dec n $ do
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


enumSuffix :: I TSuffix -> State Int (I TSuffix)
enumSuffix n@Node {node = TSuffix {..}} = dec n $ do
    l' <- enumLabel l
    c' <- enumMaybe enumTerminal c
    return $ TSuffix l' c'


-- TODO
enumTerminal :: I TTerminal -> State Int (I TTerminal)
enumTerminal n@Node {..} = undefined


enumHead :: I THead -> State Int (I THead)
enumHead n@Node {node = THead {..}} = dec n $ do
    dots' <- enumMaybe enumTerminal dots
    t' <-
        case t of
            Opt3A a -> Opt3A <$> enumTerminal a
            Opt3B a -> Opt3B <$> enumHeadName a
            Opt3C a -> Opt3C <$> enumData a
    return $ THead dots' t'


enumHeadName :: I THeadName -> State Int (I THeadName)
enumHeadName n@Node {node = THeadName {..}} = dec n $ do
    name' <- enumName name
    c' <-
        case c of
            Opt2A t -> Opt2A <$> enumTerminal t
            Opt2B t -> Opt2B <$> enumMaybe enumTerminal t
    return $ THeadName name' c'


enumData :: I TData -> State Int (I TData)
enumData n@Node {node = TData {..}} = dec n $ do
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


enumBool :: I TBool -> State Int (I TBool)
enumBool n@Node {..} = dec n $ return node


enumText :: I TText -> State Int (I TText)
enumText n@Node {..} = dec n $ return node


enumHex :: I THex -> State Int (I THex)
enumHex n@Node {..} = dec n $ return node


enumString :: I TString -> State Int (I TString)
enumString n@Node {..} = dec n $ return node


enumFloat :: I TFloat -> State Int (I TFloat)
enumFloat n@Node {..} = dec n $ return  node


enumInt :: I TInt -> State Int (I TInt)
enumInt n@Node {..} = dec n $ return node


enumBytes :: I TBytes -> State Int (I TBytes)
enumBytes n@Node {node = TBytes {..}} = dec n $ do
    bs' <-
            case bs of
                Opt3A t -> Opt3A <$> enumTerminal t
                Opt3B t -> Opt3B <$> enumByte t
                Opt3C t -> Opt3C <$> mapM enumLineBytes t
    return $ TBytes bs'


enumChar1 :: I TChar -> State Int (I TChar)
enumChar1 n = dec n $ return $ node n


enumRegex :: I TRegex -> State Int (I TRegex)
enumRegex n = dec n $ return $ node n



enumLineBytes :: I TLineBytes -> State Int (I TLineBytes)
enumLineBytes n@Node {node = m@TLineBytes {..}} = dec n $ do
    bs' <- mapM enumByte bs
    return $ TLineBytes bs'



enumByte :: I TByte -> State Int (I TByte)
enumByte n = dec n $ return $ node n