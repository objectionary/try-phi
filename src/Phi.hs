{-# LANGUAGE LambdaCase #-}
module Phi where

type Ident = String

data Attr
  = AttrIdent Ident
  | AttrPhi
  | AttrDelta
  | AttrRho
  deriving (Eq, Show)

data Term d
  = Var Ident
  | App (Term d) (Term d)
  | Object [(Attr, Maybe (Term d))]    -- ^
  | Dot (Term d) Attr                  -- ^ example.ident
  | Rho
  | BigPhi
  | SmallPhi
  | Xi
  | Data d
  deriving (Eq, Show)

data DataGraph

discover :: Term d -> Attr -> [Term d] -> Term d
discover l a s =
  case l of
    Dot l' a' ->
      discover (discover l' a' s) a s
    Xi ->
      case s of
        self : _ -> self
        _        -> error "using 𝜉 outside of objects"
    Rho ->
      case s of
        _self : parent : _ -> parent
        _                  -> error "using 𝜌 without parent"
    Object attrs ->
      case lookup a attrs of
        Just (Just v') -> v'
        Just Nothing   -> error ("referencing free attribute: " ++ show a)
        Nothing        ->
          case lookup AttrPhi attrs of
            Just (Just v') -> discover v' a (v' : s)
            _              -> error "_|_"

dataize :: Show d => Term d -> d
dataize term =
  case discover term AttrDelta [] of
    Data x -> x
    o      -> error ("Cannot dataize object: " ++ show o)

mkData :: d -> Term d
mkData x = Object [(AttrDelta, Just (Data x))]

point :: Term Double
point = Object
  [
  ]
