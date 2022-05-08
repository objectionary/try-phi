{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module EO.EOtoPhi(toMinimalTerm) where

import Data.Bifunctor (Bifunctor (second))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Maybe (fromJust, isJust)
import Data.Text (unpack)
import EOParser as EO
  ( Ann (..),
    AttachedName (..),
    HasName (..),
    Label (..),
    MethodName (..),
    Options2 (..),
    SuffixName (..),
    THeadTerminal(..),
    Ann (..),
    AttachedName (..),
    AttachedOrArgument (AttachedOrArgument),
    HasName (..),
    K,
    Label (..),
    MethodName (..),
    Options2 (Opt2B),
    Options3 (..),
    SuffixName (..),
    Term (App, Dot, HeadTerm, Obj),
    Head(..),
    HeadName(..),
    LetterName(..),
    Modifier(..), DataValue (..)
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
toMinimalTerm :: EO.K EO.Term -> Min.Term
toMinimalTerm EO.Ann {..} =
  case term of
    -- FIXME somehow use the names
    EO.App (EO.AttachedOrArgument t ns) as -> t2
      where
        t1 = toMinimalTerm t
        g (EO.AttachedOrArgument t3 xs) =
          -- TODO report errors
          case filter (\case Opt2B _ -> True; _ -> False) xs of
            [] -> Nothing
            Opt2B (Just (Ann y _)) : _ ->
              (,) (toMinimalTerm t3)
                <$> case y of
                  -- TODO use special names for attributes
                  -- not just @ and all others
                  HName y' -> Just (unpack y')
                  HAt -> Just "@"
            _ -> Nothing
        -- get application arguments having a name
        as1 = fromJust <$> filter isJust (g <$> as)
        t2 = foldl (\p1 (p2, a) -> Min.App p1 (a, p2)) t1 as1
    EO.Obj fs as -> obj
      where
        fs' =
          filter (\(Ann t _) -> case t of LVarArg _ -> False; _ -> True) fs
            <&> (\(Ann t _) -> case t of LName txt -> unpack txt; LAt -> "@"; _ -> error "LVarArg here!")
            <&> (,VoidAttr)
        g (EO.AttachedOrArgument t3 xs) =
          -- TODO partition to not allow incorrect attachments
          case filter (\case Opt2A _ -> True; _ -> False) xs of
            [] -> Nothing
            Opt2A (AttachedName (SuffixName (Ann y _) _) _) : _ ->
              (,toMinimalTerm t3)
                <$> case y of
                  LName s -> Just $ unpack s
                  LAt -> Just "@"
                  LVarArg _ -> Nothing
            _ -> Nothing
        as1 = as <&> g & filter isJust <&> fromJust <&> second Attached
        obj = Min.Obj $ Object (InsOrdHashMap.fromList (fs' <> as1))

    -- FIXME somehow use the names
    EO.Dot (EO.AttachedOrArgument t ns) as -> t1
      where
        t' = toMinimalTerm t
        as' =
          as
            <&> ( \(Ann m _) ->
                    case m of
                      MName tx -> unpack tx
                      MRho -> "^"
                      MAt -> "@"
                      MVertex -> ">"
                )
        t1 = foldl Min.Dot t' as'
    EO.HeadTerm l h -> t
      where
        loc = l <&> Loc
        h' = h <&> \(Ann (Head x _) _) ->
          case x of
            Opt3A (Ann a _) -> Left $
              case a of
                -- FIXME use another type for head expressions
                -- it shouldn't include ^
                HeadRho -> "^"
                  -- error "head term shouldn't contain ^"
                HeadRoot -> "Q"
                HeadXi -> error "head term shouldn't contain $"
                HeadSigma -> "&"
                HeadStar -> "*"
                HeadAt -> "@"
            Opt3B (Ann (HeadName (Ann (LetterName tx) _) m) _) -> Left $
              unpack tx <> maybe "" (\case MCopy -> "'"; MInverseDot -> ".") m
            Opt3C (Ann a _) -> Right . DataTerm $
              case a of
                DInt i -> DataInteger i
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
