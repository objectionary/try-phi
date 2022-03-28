{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module EnumerateNodes (enumInsertProgram) where

import           Control.Monad.State.Strict (State, get, put, runState)
import qualified Data.HashMap.Strict.InsOrd as M (InsOrdHashMap, empty, insert)
import           ParseEO

-- enumerate nodes
-- put their ids as keys and nodes as values into a map

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
    | MDots (I TDots)
    | MConst (I TConst)
    | MFreeAttribute (I TFreeAttribute)
    | MVarArg (I TVarArg)

data MyState = MyState {i::Int, m::M.InsOrdHashMap Int MapElement}
dec::I a -> (I a -> MapElement) -> State MyState a -> State MyState (I a)
dec n m p = do
    t <- p
    MyState {i=i1, m=m1} <- get
    put MyState {i=i1+1, m=M.insert i1 (m n) m1}
    return n {load = Load {num = i1}, node = t}

enumInsertProgram :: I TProgram -> (I TProgram, MyState)
enumInsertProgram t = runState (enum t) (MyState {i=0, m=M.empty})

-- enums

class Enumerable a where
    enum :: a -> State MyState a

instance Enumerable a => Enumerable (Maybe a) where
    enum (Just x) = Just <$> enum x
    enum Nothing = return Nothing

instance Enumerable (I TProgram) where
    enum n@Node {node = TProgram {..}} = dec n MProgram $ do
        l' <- enum l
        m' <- enum m
        o' <- enum o
        return $ TProgram l' m' o'

instance Enumerable (I TLicense) where
    enum n@Node {node = TLicense {..}} = dec n MLicense $ do
        cs' <- mapM enum cs
        return $ TLicense cs'

instance Enumerable (I TComment) where
    enum n@Node {..} = dec n MComment $ return node

instance Enumerable (I TMetas ) where
    enum n@Node {node = TMetas {..}} = dec n MMetas $ do
        cs' <- mapM enum ms
        return $ TMetas cs'

instance Enumerable (I TMeta) where
    enum n@Node {node = TMeta {..}} = dec n MMeta $ do
        name' <- enum name
        suff' <- enum suff
        return $ TMeta name' suff'

instance Enumerable (I TMetaSuffix) where
    enum n@Node {..} = dec n MMetaSuffix $ return node

instance Enumerable (I TName ) where
    enum m@Node {..} = dec m MName $ return node

instance Enumerable (I TObjects) where
    enum n@Node {node = TObjects {..}} = dec n MObjects $ do
        os' <- mapM enum os
        return $ TObjects os'

instance Enumerable (I TObject) where
    enum n@Node {node = TObject {..}} = dec n MObject $ do
        cs' <- mapM enum cs
        a' <- enum a
        t' <- enum t
        -- TODO
        let
            g (m,h,suff,t1) =
                do
                    m1 <- enum m
                    h1 <- enum h
                    s1 <- enum suff
                    t2 <- enum t1
                    return (m1,h1,s1,t2)
        s' <- mapM g s
        return $ TObject cs' a' t' s'

instance Enumerable (I TAbstraction) where
    enum n@Node {node = TAbstraction {..}} = dec n MAbstraction $ do
        as' <- enum as
        t' <- enum t
        return $ TAbstraction as' t'

instance Enumerable (I TTail ) where
    enum n@Node {node = TTail {..}} = dec n MTail $ do
        os' <- mapM enum os
        return $ TTail os'

instance (Enumerable a, Enumerable b) => Enumerable (Options2  a b) where
    enum (Opt2A a) = Opt2A <$> enum a
    enum (Opt2B a) = Opt2B <$> enum a

instance Enumerable (I TApplication) where
    enum n@Node {node = TApplication {..}} = dec n MApplication $ do
        s' <- enum s
        h' <- enum h
        a1' <- enum a1
        return $ TApplication s' h' a1'

instance Enumerable (I TApplication1) where
    enum n@Node {node = TApplication1 {..}} = dec n MApplication1 $ do
        c' <- enum c
        return $ TApplication1 c'

instance Enumerable (I TApplication1Elem) where
    enum n@Node {node = TApplication1Elem {..}} = dec n MApplication1Elem $ do
        c1' <- enum c1
        ht' <- enum ht
        a' <- enum a
        return $ TApplication1Elem c1' ht' a'

instance Enumerable (I TMethod) where
    enum n@Node {node = TMethod {..}} = dec n MMethod $ do
        m' <- case m of
            Opt2A t -> Opt2A <$> enum t
            Opt2B t -> Opt2B <$> enum t
        return $ TMethod m'

instance Enumerable (I THas) where
    enum m@Node {node = THas {..}} = dec m MHas $ do
        n' <- enum n
        return $ THas n'

instance Enumerable (I TAttributes) where
    enum n@Node {node = TAttributes {..}} = dec n MAttributes $ do
        as' <- mapM enum as
        return $ TAttributes as'

instance Enumerable (I TAbstractionTail) where
    enum n@Node {node = TAbstractionTail {..}} = dec n MAbstractionTail $ do
        e' <-
            case e of
                Opt2A (a,b) -> Opt2A <$> do
                    a1 <- enum a
                    b1 <- (
                        case b of
                            Just b' -> Just <$>
                                case b' of
                                    Opt2A name -> Opt2A <$> enum name
                                    Opt2B t    -> Opt2B <$> enum t
                            Nothing -> return b
                        )
                    return (a1,b1)
                Opt2B h -> Opt2B <$> enum h
        return $ TAbstractionTail e'

instance Enumerable (I THtail) where
    enum n@Node {node = THtail {..}} = dec n MHtail $ do
        let f e =
                case e of
                    Opt3A a -> Opt3A <$> enum a
                    Opt3B a -> Opt3B <$> enum a
                    Opt3C a -> Opt3C <$> enum a
        t' <- mapM f t
        return $ THtail t'

instance Enumerable (I TLabel) where
    enum n@Node {node = TLabel {..}} = dec n MLabel $ do
        l' <-
            case l of
                Opt2A t -> Opt2A <$> enum t
                -- TODO
                Opt2B (n1, t) ->
                    do
                        n1' <- enum n1
                        t' <- enum t
                        return $ Opt2B (n1', t')
        return $ TLabel l'

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (Options3 a b c) where
    enum (Opt3A a) = Opt3A <$> enum a
    enum (Opt3B a) = Opt3B <$> enum a
    enum (Opt3C a) = Opt3C <$> enum a

instance Enumerable (I TFreeAttribute) where
    enum n@Node {node = TFreeAttribute {..}} = dec n MFreeAttribute $ do
        l' <- enum l
        return $ TFreeAttribute l'

instance Enumerable (I TSuffix) where
    enum n@Node {node = TSuffix {..}} = dec n MSuffix $ do
        l' <- enum l
        c' <- enum c
        return $ TSuffix l' c'

instance Enumerable (I TVarArg) where
    enum n@Node {..} = dec n MVarArg $ return node

instance Enumerable (I TConst) where
    enum n@Node {..} = dec n MConst $ return node

instance Enumerable (I TTerminal) where
    enum n@Node {..} = dec n MTerminal $ return node

instance Enumerable (I TDots) where
    enum n@Node {..} = dec n MDots $ return node

instance Enumerable (I THead) where
    enum n@Node {node = THead {..}} = dec n MHead $ do
        dots' <- enum dots
        t' <-
            case t of
                Opt3A a -> Opt3A <$> enum a
                Opt3B a -> Opt3B <$> enum a
                Opt3C a -> Opt3C <$> enum a
        return $ THead dots' t'

instance Enumerable (I THeadName) where
    enum n@Node {node = THeadName {..}} = dec n MHeadName $ do
        name' <- enum name
        c' <-
            case c of
                Opt2A t -> Opt2A <$> enum t
                Opt2B t -> Opt2B <$> enum t
        return $ THeadName name' c'

instance (Enumerable a, Enumerable b,Enumerable c,Enumerable d,Enumerable e,Enumerable f,Enumerable g,Enumerable h,Enumerable i)  => Enumerable (Options9 a b c d e f g h i) where
    enum (Opt9A a) = Opt9A <$> enum a
    enum (Opt9B a) = Opt9B <$> enum a
    enum (Opt9C a) = Opt9C <$> enum a
    enum (Opt9D a) = Opt9D <$> enum a
    enum (Opt9E a) = Opt9E <$> enum a
    enum (Opt9F a) = Opt9F <$> enum a
    enum (Opt9G a) = Opt9G <$> enum a
    enum (Opt9H a) = Opt9H <$> enum a
    enum (Opt9I a) = Opt9I <$> enum a

instance Enumerable (I TData) where
    enum n@Node {node = TData {..}} = dec n MData $ do
        d' <- enum d                
        return $ TData d'

instance Enumerable (I TBool) where
    enum n@Node {..} = dec n MBool $ return node

instance Enumerable (I TText) where
    enum n@Node {..} = dec n MText $ return node

instance Enumerable (I THex) where
    enum n@Node {..} = dec n MHex $ return node

instance Enumerable (I TString) where
    enum n@Node {..} = dec n MString $ return node

instance Enumerable (I TFloat) where
    enum n@Node {..} = dec n MFloat $ return  node

instance Enumerable (I TInt) where
    enum n@Node {..} = dec n MInt $ return node

instance Enumerable (I TBytes) where
    enum n@Node {node = TBytes {..}} = dec n MBytes $ do
        bs' <-
                case bs of
                    Opt2A t -> Opt2A <$> enum t
                    Opt2B t -> Opt2B <$> mapM enum t
        return $ TBytes bs'

instance Enumerable (I TChar) where
    enum n = dec n MChar $ return $ node n

instance Enumerable (I TRegex) where
    enum n = dec n MRegex $ return $ node n

instance Enumerable (I TLineBytes) where
    enum n@Node {node = TLineBytes {..}} = dec n MLineBytes $ do
    bs' <- mapM enum bs
    return $ TLineBytes bs'

instance Enumerable (I TByte) where
    enum n = dec n MByte $ return $ node n
