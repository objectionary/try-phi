{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ToTermTH (Ann (..), EpiAnn (..), genEpiN) where

import Data.Data (Data)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Pretty.Simple
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

newtype Ann = Ann {num :: Int} deriving (Data, Eq, Ord, Hashable, Generic)

class EpiAnn a where
  modify :: (Ann -> Ann) -> a -> a
  get :: a -> Ann

--FIXME show segment
instance Show Ann where
  show Ann {..} = "{ " <> show num <> " }"

data K = K {ann :: Ann}


instance EpiAnn K where
  modify f x = x {ann = f ((ann :: K -> Ann) x)}
  get x = ann x

ppQ :: (Quasi m, Show a) => Q a -> m ()
ppQ x = runQ x >>= pPrint

genEpi :: Name -> Q Dec
genEpi t =
  do
    let f = mkName "f"
        x = mkName "x"
        ann = mkName "ann"
    return $
      InstanceD
        Nothing
        []
        (AppT (ConT ''EpiAnn) (ConT t))
        [ FunD
            'modify
            [ Clause
                [ VarP f,
                  VarP x
                ]
                ( NormalB
                    ( RecUpdE
                        (VarE x)
                        [ ( ann,
                            AppE
                              (VarE f)
                              ( AppE
                                  ( SigE
                                      (VarE ann)
                                      ( AppT
                                          (AppT ArrowT (ConT t))
                                          (ConT ''Ann)
                                      )
                                  )
                                  (VarE x)
                              )
                          )
                        ]
                    )
                )
                []
            ],
          FunD
            'get
            [ Clause
                [VarP x]
                ( NormalB
                    ( AppE
                    ( SigE ( VarE ann )
                        ( AppT
                            ( AppT ArrowT ( ConT t ) ) ( ConT ''Ann )
                        )
                    ) ( VarE x )
                )
                )
                []
            ]
        ]

genEpiN :: [Name] -> Q [Dec]
genEpiN = traverse genEpi
