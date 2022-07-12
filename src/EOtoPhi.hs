{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module EOtoPhi(toMinimalTerm) where

import Data.Bifunctor (Bifunctor (second))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Maybe (fromJust, isJust)
import Data.Text (unpack)
import EOParser as EO
  ( AttachedName (..),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (..),
    SuffixName (..),
    HeadTerminal(..),
    AttachedName (..),
    AttachedOrArgument (..),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (..),
    Options3 (..),
    SuffixName (..),
    Term (..),
    Head(..),
    HeadName(..),
    LetterName(..),
    Modifier(..),
    DataValue (..),
  )
import Phi.Minimal.Model as Min

-- |
-- For now, will support simple expressions like
--
-- > eo { p a:b c } -> phi { p(b -> a)(_ -> c) }
-- >
-- > eo {
-- > [b] > a
-- >   4 > t
-- > } ->
-- > phi { a -> [t -> 4, b -> ?] }
-- >
-- > eo { a.b } -> phi { a.b }
toMinimalTerm :: EO.Term -> Min.Term
toMinimalTerm = \case
    -- FIXME somehow use the names
    EO.App {t = EO.AttachedOrArgument {t = t, a = ns}, args = as} -> t2
      where
        t1 = toMinimalTerm t
        g EO.AttachedOrArgument {t = t3, a = xs} =
          -- TODO report errors
          case filter (\case Opt2B _ -> True; _ -> False) xs of
            [] -> Nothing
            Opt2B (Just y) : _ ->
              (,) (toMinimalTerm t3)
                <$> case y of
                  -- TODO use special names for attributes
                  -- not just @ and all others
                  HName {t = y'} -> Just (unpack y')
                  HAt {} -> Just "@"
            _ -> Nothing
        -- get application arguments having a name
        as1 = fromJust <$> filter isJust (g <$> as)
        t2 = foldl (\p1 (p2, a) -> Min.App p1 (a, p2)) t1 as1
    EO.Obj {freeAttrs = fs, attrs = as} -> obj
      where
        fs' =
          filter (\case LVarArg {} -> False; _ -> True) fs
            <&> (\case LName {n = txt} -> unpack txt; LAt {} -> "@"; _ -> error "LVarArg here!")
            <&> (,VoidAttr)
        g EO.AttachedOrArgument {t = t3, a = xs} =
          -- TODO partition to not allow incorrect attachments
          case filter (\case Opt2A _ -> True; _ -> False) xs of
            [] -> Nothing
            -- TODO support imports
            -- TODO handle list end
            Opt2A AttachedName {a = SuffixName {n = y}} : _ ->
              (,toMinimalTerm t3)
                <$> case y of
                  LName {n = s} -> Just $ unpack s
                  LAt {} -> Just "@"
                  -- FIXME has some text
                  LVarArg {} -> Nothing
            _ -> Nothing
        as1 = as <&> g & filter isJust <&> fromJust <&> second Attached
        obj = Min.Obj $ Object (InsOrdHashMap.fromList (fs' <> as1))

    -- FIXME somehow use the names
    EO.Dot {t = EO.AttachedOrArgument {t = t, a = ns},  attr = as} -> t1
      where
        t' = toMinimalTerm t
        as' =
          as
            <&> ( \case
                      MName {n} -> unpack n
                      MRho {} -> "^"
                      MAt {} -> "@"
                      MVertex {} -> ">"
                )
        t1 = foldl Min.Dot t' as'
    EO.HeadTerm {n = l, a = h} -> t
      where
        loc = l <&> Loc
        h' = h <&> \Head {h = x} ->
          case x of
            Opt3A a -> Left $
              case a of
                -- FIXME use another type for head expressions
                -- it shouldn't include ^
                HeadRho {} -> "^"
                  -- error "head term shouldn't contain ^"
                HeadRoot {} -> "Q"
                -- FIXME throw exception
                HeadXi {} -> error "head term shouldn't contain $"
                HeadSigma {} -> "&"
                HeadStar {} -> "*"
                HeadAt {} -> "@"
            Opt3B HeadName {n = LetterName {n = tx}, m = m} -> Left $
              unpack tx <> maybe "" (\case MCopy {} -> "'"; MInverseDot {} -> ".") m
            Opt3C a -> Right . DataTerm $
              case a of
                DInt {i} -> DataInteger i
                -- TODO support more data types
                i -> NoData
        t =
          case (loc, h') of
            (Just a, Just b) -> Min.Dot a $
              case b of
                Left b' -> b'
                Right d -> error "No locator before data allowed"
            (Just a, _) -> a
            (_, Just b) ->
              case b of
                Left b' -> Min.Dot (Loc 0) b'
                Right b' -> b'
            (_, _) -> error "Locator and data cannot be simultaneously Nothing"
