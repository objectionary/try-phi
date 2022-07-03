{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Phi.Minimal.Model.Arbitrary where

import           Data.Bifunctor             (first)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           GHC.Exts                   (fromList)
import           Test.QuickCheck

import           Phi.Minimal.Model

-- | This instance generates closed term
-- (meaning that each locator references an object within a term).
instance Arbitrary Term where
  arbitrary = genTerm 0
  shrink = shrinkTerm

instance Arbitrary a => Arbitrary (AttrValue a) where
  arbitrary = oneof
    [ pure VoidAttr
    , Attached <$> arbitrary ]

newtype Attr' = Attr' { getAttr' :: Attr }

instance Arbitrary Attr' where
  arbitrary = pure (Attr' "arbitrary")
  shrink _ = []

shrinkTerm :: Term -> [Term]
shrinkTerm = \case
  Loc n
    | n > 0     -> [Loc (n - 1)]
    | otherwise -> []
  Obj o -> Obj . fromList <$> (map (first getAttr') <$> shrink (first Attr' <$> InsOrdHashMap.toList (getObject o)))
  App t (a, u) -> concat
    [ [t, u]
    , map (`App` (a, u)) (shrink t)
    , map (App t) ((a,) <$> shrink u)
    ]
  Dot t a -> [t] ++ map (`Dot` a) (shrink t)

genTerm :: Int -> Gen Term
genTerm n = frequency $ concat
  [ if n > 0 then [ (n, Loc <$> choose (0, n - 1)) ] else []
  , if n < 6 then [ (1, App <$> genTerm n <*> ((,) <$> genVoidAttr <*> genTerm n)) ] else []
  , [ (2, Obj <$> genObject n)
    , (1, Dot <$> genTerm n <*> genAttr)
    ]
  ]

voidAttrNames :: [Attr]
voidAttrNames = [ "v" <> show n | n <- [1..3] ]

attachedAttrNames :: [Attr]
attachedAttrNames = [ "a" <> show n | n <- [1..3] ]

genObject :: Int -> Gen (Object Term)
genObject n
  | n > 5 = return (fromList [])
  | otherwise = do
      void <- fmap (,VoidAttr) <$> sublistOf voidAttrNames
      attached <- sublistOf attachedAttrNames >>= mapM mkAttachedAttr
      return $ fromList (void ++ attached)
  where
    mkAttachedAttr a = (a,) . Attached <$> genTerm (n+1)

genAttr :: Gen Attr
genAttr = oneof
  [ elements voidAttrNames
  , elements attachedAttrNames ]

genVoidAttr :: Gen Attr
genVoidAttr = elements voidAttrNames
