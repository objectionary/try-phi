{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module EnumerateNodes (enumInsertProgram, getIndexedProgram) where

import Control.Monad.State.Strict (State, evalState, get, put, runState)
import qualified Data.HashMap.Strict.InsOrd as M (InsOrdHashMap, empty, insert)
import ParseEO

-- enumerate nodes
-- put their ids as keys and nodes as values into a map

data MapElement
  = MProgram TProgram
  | MLicense TLicense
  | MComment TComment
  | MMetas TMetas
  | MMeta TMeta
  | MMetaSuffix TMetaSuffix
  | MName TName
  | MObjects TObjects
  | MObject TObject
  | MAbstraction TAbstraction
  | MTail TTail
  | MApplication TApplication
  | MApplication1 TApplication1
  | MApplication1Elem TApplication1Elem
  | MMethod TMethod
  | MHas THas
  | MAttributes TAttributes
  | MAbstractionTail TAbstractionTail
  | MHtail THtail
  | MLabel TLabel
  | MSuffix TSuffix
  | MHead THead
  | MHeadName THeadName
  | MData TData
  | MBool TBool
  | MText TText
  | MHex THex
  | MString TString
  | MFloat TFloat
  | MInt TInt
  | MBytes TBytes
  | MChar TChar
  | MRegex TRegex
  | MLineBytes TLineBytes
  | MByte TByte
  | MDots TDots
  | MConst TConst
  | MFreeAttribute TFreeAttribute
  | MVarArg TVarArg
  | MHeadTerminal THeadTerminal
  | MMethodTerminal TMethodTerminal
  | MLabelTerminal TLabelTerminal
  | MHeadModifier THeadModifier
  | MAbstrQuestion TAbstrQuestion
  | MObjectTail TObjectTail

data MyState = MyState {i :: Int, m :: M.InsOrdHashMap Int MapElement}

-- instance Traversable TProgram where
--     trav

-- | conduct common operations on a node
-- change annotation of a node by taking
--    tree id from state
-- insert a mapping between the tree id and tree node into map
dec :: EpiAnn a => a -> (a -> MapElement) -> State MyState a -> State MyState a
dec pt m s = do
  t <- s
  MyState {i = i1, m = m1} <- get
  put MyState {i = i1 + 1, m = M.insert i1 (m pt) m1}
  return (modify (\a -> a {num = i1}) t)

enumInsertProgram :: TProgram -> (TProgram, MyState)
enumInsertProgram t = runState (enum t) (MyState {i = 0, m = M.empty})

getIndexedProgram :: TProgram -> TProgram
getIndexedProgram t = evalState (enum t) (MyState {i = 0, m = M.empty})

-- enums

class Enumerable a where
  enum :: a -> State MyState a

instance Enumerable a => Enumerable (Maybe a) where
  enum (Just x) = Just <$> enum x
  enum Nothing = return Nothing

instance Enumerable TProgram where
  enum pt@TProgram {..} = dec pt MProgram $ do
    l' <- enum l
    m' <- enum m
    o' <- enum o
    return $ pt {l = l', m = m', o = o'}

instance Enumerable TLicense where
  enum pt@TLicense {..} = dec pt MLicense $ do
    cs' <- mapM enum cs
    return (pt {cs = cs'} :: TLicense)

instance Enumerable TMetas where
  enum pt@TMetas {..} = dec pt MMetas $ do
    ms' <- mapM enum ms
    return pt {ms = ms'}

instance Enumerable TMeta where
  enum pt@TMeta {..} = dec pt MMeta $ do
    name' <- enum name
    suff' <- enum suff
    return $ pt {name = name', suff = suff'}

instance Enumerable TObjects where
  enum pt@TObjects {..} = dec pt MObjects $ do
    os' <- mapM enum os
    return (pt {os = os'} :: TObjects)

instance Enumerable TObject where
  enum pt@TObject {..} = dec pt MObject $ do
    cs' <- mapM enum cs
    a' <- enum a
    t' <- enum t
    -- TODO
    let g pt1@TObjectTail {m = m, h = h, s = suff, t = t1} = dec pt1 MObjectTail $
          do
            m1 <- enum m
            h1 <- enum h
            s1 <- enum suff
            t2 <- enum t1
            return pt1 {m = m1, h = h1, s = s1, t = t2}
    s' <- mapM g s
    return $ pt {cs = cs', a = a', t = t', s = s'}

instance Enumerable TAbstraction where
  enum pt@TAbstraction {..} = dec pt MAbstraction $ do
    as' <- enum as
    t' <- enum t
    return $ pt {as = as', t = t'}

instance Enumerable TTail where
  enum pt@TTail {..} = dec pt MTail $ do
    os' <- mapM enum os
    return $ (pt {os = os'} :: TTail)

instance (Enumerable a, Enumerable b) => Enumerable (Options2 a b) where
  enum (Opt2A a) = Opt2A <$> enum a
  enum (Opt2B a) = Opt2B <$> enum a

instance Enumerable TApplication where
  enum pt@TApplication {..} = dec pt MApplication $ do
    s' <- enum s
    h' <- enum h
    a1' <- enum a1
    return $ TApplication s' h' a1' ann

instance Enumerable TApplication1 where
  enum pt@TApplication1 {..} = dec pt MApplication1 $ do
    c' <- enum c
    return (pt {c = c'} :: TApplication1)

instance Enumerable TApplication1Elem where
  enum pt@TApplication1Elem {..} = dec pt MApplication1Elem $ do
    c1' <- enum c1
    ht' <- enum ht
    a' <- enum a
    return $ pt {c1 = c1', ht = ht', a = a'}

instance Enumerable TMethod where
  enum pt@TMethod {..} = dec pt MMethod $ do
    m' <- case m of
      Opt2A t -> Opt2A <$> enum t
      Opt2B t -> Opt2B <$> enum t
    return (pt {m = m'} :: TMethod)

instance Enumerable THas where
  enum pt@THas {..} = dec pt MHas $ do
    n' <- enum n
    return (pt {n = n'} :: THas)

instance Enumerable TAttributes where
  enum pt@TAttributes {..} = dec pt MAttributes $ do
    as' <- mapM enum as
    return (pt {as = as'} :: TAttributes)

instance Enumerable TAbstractionTail where
  enum pt@TAbstractionTail {..} = dec pt MAbstractionTail $ do
    e' <-
      case e of
        Opt2A (a, b) ->
          Opt2A <$> do
            a1 <- enum a
            b1 <-
              ( case b of
                  Just b' ->
                    Just
                      <$> case b' of
                        Opt2A name -> Opt2A <$> enum name
                        Opt2B t -> Opt2B <$> enum t
                  Nothing -> return b
                )
            return (a1, b1)
        Opt2B h -> Opt2B <$> enum h
    return $ pt {e = e'}

instance Enumerable THtail where
  enum pt@THtail {..} = dec pt MHtail $ do
    t' <- mapM enum t
    return (pt {t = t'} :: THtail)

instance Enumerable TLabel where
  enum pt@TLabel {..} = dec pt MLabel $ do
    l' <-
      case l of
        Opt2A t -> Opt2A <$> enum t
        -- TODO
        Opt2B (n1, t) ->
          do
            n1' <- enum n1
            t' <- enum t
            return $ Opt2B (n1', t')
    return (pt {l = l'} :: TLabel)

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (Options3 a b c) where
  enum (Opt3A a) = Opt3A <$> enum a
  enum (Opt3B a) = Opt3B <$> enum a
  enum (Opt3C a) = Opt3C <$> enum a

instance Enumerable TFreeAttribute where
  enum pt@TFreeAttribute {..} = dec pt MFreeAttribute $ do
    l' <- enum l
    return $ (pt {l = l'} :: TFreeAttribute) 

instance Enumerable TSuffix where
  enum pt@TSuffix {..} = dec pt MSuffix $ do
    l' <- enum l
    c' <- enum c
    return $ pt {l = l', c = c'}

instance Enumerable THead where
  enum pt@THead {..} = dec pt MHead $ do
    dots' <- enum dots
    t' <-
      case t of
        Opt3A a -> Opt3A <$> enum a
        Opt3B a -> Opt3B <$> enum a
        Opt3C a -> Opt3C <$> enum a
    return $ pt {dots = dots', t = t'}

instance Enumerable THeadModifier where
  enum pt = dec pt MHeadModifier $ return pt

instance Enumerable THeadName where
  enum pt@THeadName {..} = dec pt MHeadName $ do
    name' <- enum name
    c' <- enum c
    return $ pt {name = name', c = c'}

instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f, Enumerable g, Enumerable h, Enumerable i) => Enumerable (Options9 a b c d e f g h i) where
  enum (Opt9A a) = Opt9A <$> enum a
  enum (Opt9B a) = Opt9B <$> enum a
  enum (Opt9C a) = Opt9C <$> enum a
  enum (Opt9D a) = Opt9D <$> enum a
  enum (Opt9E a) = Opt9E <$> enum a
  enum (Opt9F a) = Opt9F <$> enum a
  enum (Opt9G a) = Opt9G <$> enum a
  enum (Opt9H a) = Opt9H <$> enum a
  enum (Opt9I a) = Opt9I <$> enum a

instance Enumerable TData where
  enum pt@TData {..} = dec pt MData $ do
    d' <- enum d
    return $ pt {d = d'} 

instance Enumerable TBytes where
  enum pt@TBytes {..} = dec pt MBytes $ do
    bs' <-
      case bs of
        Opt2A t -> Opt2A <$> enum t
        Opt2B t -> Opt2B <$> mapM enum t
    return (pt {bs = bs'} :: TBytes)

instance Enumerable TLineBytes where
  enum pt@TLineBytes {..} = dec pt MLineBytes $ do
    bs' <- mapM enum bs
    return (pt {bs = bs'} :: TLineBytes)

instance Enumerable TChar where
  enum pt = dec pt MChar $ return pt

instance Enumerable TRegex where
  enum pt = dec pt MRegex $ return pt

instance Enumerable TByte where
  enum pt = dec pt MByte $ return pt

instance Enumerable TBool where
  enum pt = dec pt MBool $ return pt

instance Enumerable TText where
  enum pt = dec pt MText $ return pt

instance Enumerable THex where
  enum pt = dec pt MHex $ return pt

instance Enumerable TString where
  enum pt = dec pt MString $ return pt

instance Enumerable TFloat where
  enum pt = dec pt MFloat $ return pt

instance Enumerable TInt where
  enum pt = dec pt MInt $ return pt

instance Enumerable TVarArg where
  enum pt = dec pt MVarArg $ return pt

instance Enumerable TConst where
  enum pt = dec pt MConst $ return pt

instance Enumerable TDots where
  enum pt = dec pt MDots $ return pt

instance Enumerable TMethodTerminal where
  enum pt = dec pt MMethodTerminal $ return pt

instance Enumerable TLabelTerminal where
  enum pt = dec pt MLabelTerminal $ return pt

instance Enumerable THeadTerminal where
  enum pt = dec pt MHeadTerminal $ return pt

instance Enumerable TAbstrQuestion where
  enum pt = dec pt MAbstrQuestion $ return pt

instance Enumerable TMetaSuffix where
  enum pt = dec pt MMetaSuffix $ return pt

instance Enumerable TName where
  enum pt = dec pt MName $ return pt

instance Enumerable TComment where
  enum pt = dec pt MComment $ return pt