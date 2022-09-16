{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ParseEOTH (Position (..), Ann (..), Segment (..), EpiAnn (..), genEpiN) where

import Data.Data (Data)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Pretty.Simple
import Text.Printf (printf)

data Position = Position
  { row :: Int,
    column :: Int
  }
  deriving (Data)

data Ann = Ann {num :: Int, segment :: Segment}
  deriving (Data)

data Segment = Segment {start :: Position, end :: Position} deriving (Data)

class EpiAnn a where
  modify :: (Ann -> Ann) -> a -> a
  get :: a -> Ann

instance Show Position where
  show (Position r c) = printf "%d:%d" r c

instance Show Segment where
  show Segment {..} = printf "[%s..%s]" (show start) (show end)

--FIXME show segment
instance Show Ann where
  show Ann {..} = "{ (" <> show num <> "), " <> show segment <> " }"

data K = K {ann :: Ann}


instance EpiAnn K where
  modify f x = x {ann = f ((ann :: K -> Ann) x)}
  get x = ann x

ppQ :: (Quasi m, Show a) => Q a -> m ()
ppQ x = runQ x >>= pPrint

{-
>>>ppQ [d| instance EpiAnn K where {modify f x = x {ann = f ((ann:: K -> Ann) x)}; get x = (ann :: K -> Ann) x} |]
-}

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
