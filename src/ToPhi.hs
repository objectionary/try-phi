{-# LANGUAGE DeriveDataTypeable #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module ToPhi where


import ToTerm ()


{-
What can we reuse?

-}

type AttrName = String

-- newtype Object a = Object
--   { getObject :: InsOrdHashMap Attr (AttrValue a)
--   }
--   deriving (Eq, Functor, Foldable, Traversable, IsList)


-- data Term
--   = Obj (Object Term)
--   | Dot Term Attr
--   | App Term (Attr, Term)
--   | Loc Int
--   | DataTerm DataValue
-- data Term
--   = App {name :: Term, }
