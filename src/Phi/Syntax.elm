module Phi.Syntax exposing (..)

import Dict exposing (Dict)

compile : String -> String
compile s = "compiled: " ++ s

type alias Ident = String

type alias Attr = String
-- type Attr
--   = AttrIdent Ident
--   | AttrPhi
--   | AttrDelta

type alias Locator = String
-- type Locator
--   = AttrLocator Attr
--   | Rho
--   | Xi

type alias Object d = Dict Attr (Term d)

type alias Substitution d = Dict Locator (Term d)

type Term d
  = Var Locator
  | App (Term d) (Ident, Term d)
  | Object (Object d)
  | Dot (Term d) Attr
  | FreeAttr
  | Data d
  | Atom Ident (Maybe Int) (List (Object d)) (List (Object d) -> Object d)

type alias DefaultObject = Object (Result String Int)
type alias DefaultTerm = Term (Result String Int)

type Step d
  = StepErr String
  | StepTerm (Term d)
  | StepData d
  | StepSkipMany
