{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Phi.Minimal.Model.Arbitrary where

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import           GHC.Exts                   (fromList)
import           Test.QuickCheck

import           Phi.Minimal.Model

instance Arbitrary Term where
  arbitrary = genTerm 3
  shrink = shrinkTerm

instance Arbitrary a => Arbitrary (AttrValue a) where
  arbitrary = oneof
    [ pure VoidAttr
    , Attached <$> arbitrary ]

shrinkTerm :: Term -> [Term]
shrinkTerm = \case
  Loc n
    | n > 0     -> [Loc (n - 1)]
    | otherwise -> []
  Obj o -> Obj . fromList <$> shrink (InsOrdHashMap.toList (getObject o))
  App t (a, u) -> concat
    [ [t, u]
    , map (`App` (a, u)) (shrink t)
    , map (App t) ((a,) <$> shrink u)
    ]
  Dot t a -> [t] ++ map (`Dot` a) (shrink t)

genTerm :: Int -> Gen Term
genTerm n = frequency
  [ (n, Loc <$> choose (0, n - 1))
  , (1, Obj <$> genObject n)
  , (1, Dot <$> genTerm n <*> genAttr)
  , (1, App <$> genTerm n <*> ((,) <$> genVoidAttr <*> genTerm n))
  ]

voidAttrNames :: [Attr]
voidAttrNames = [ "v" <> show n | n <- [1..3] ]

attachedAttrNames :: [Attr]
attachedAttrNames = [ "a" <> show n | n <- [1..3] ]

genObject :: Int -> Gen (Object Term)
genObject n = do
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
