{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module TH where

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

data Ann
  = Ann {num :: Int, segment :: Segment}
  | Other
  deriving (Data)

data Segment = Segment {start :: Position, end :: Position} deriving (Data)

class EpiAnn a where
  setAnn :: Ann -> a -> a

instance Show Position where
  show (Position r c) = printf "%d:%d" r c

instance Show Segment where
  show Segment {..} = printf "[%s..%s]" (show start) (show end)

--FIXME show segment
instance Show Ann where
  show Ann {..} = "{ (" <> show num <> "), " <> show segment <> " }"
  show Other = "Other"


ppQ :: (Quasi m, Show a) => Q a -> m ()
ppQ x = runQ x >>= pPrint

genEpi :: Name -> Q Dec
genEpi t =
  do
    let a = mkName "a"
        b = mkName "b"
        ann = mkName "ann"
    return $
      InstanceD
          Nothing
          []
          (AppT (ConT ''EpiAnn) (ConT t))
          [ FunD
              'setAnn
              [ Clause
                  [ VarP a,
                    VarP b
                  ]
                  ( NormalB
                      ( RecUpdE
                          (VarE b)
                          [ ( ann,
                              VarE a
                            )
                          ]
                      )
                  )
                  []
              ]
          ]

genEpiN :: [Name] -> Q [Dec]
genEpiN = traverse genEpi

-- instance EpiAnn THeadTerminal where
--   setAnn a t = t {ann = a}

{-
>>>
-}