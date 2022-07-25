{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Phi.Minimal.Examples where

import Phi.Minimal.Model
  ( AttrValue (..),
    DataValue (..),
    Term (..),
  )

import Phi.Minimal.Pretty

-- * Examples

-- | Empty object.
--
-- >>> empty
-- ⟦⟧
empty :: Term
empty = Obj []

{- |
>>> ex0
3
-}
ex0 :: Term
ex0 = DataTerm $ DataInteger 3
{- |
>>> ex1
⟦
  x ↦ ρ³
⟧
-}
ex1 :: Term
ex1 = Obj [("x", Attached (Loc 3))]

{- |
>>> ex2
⟦
  x ↦ ⟦
    y ↦ ρ¹
  ⟧
⟧.x.y
-}
ex2 :: Term
ex2 = Dot (Dot (Obj [("x", Attached (Obj [("y", Attached (Loc 1))]))]) "x") "y"

{- |
>>>ex3
-}
ex3 :: Term
ex3 = Obj [("x", Attached ex1), ("y", Attached ex2)]

{- |
>>>ex20
-}
ex20 :: Term
ex20 = Obj [("@", Attached (Obj [("y", VoidAttr), ("z", VoidAttr)]))]


{- |
>>>ex4
-}
ex4 :: Term
ex4 = Obj [("x", Attached ex3), ("y", Attached ex2), ("z", Attached ex1)]

ex5 :: Term
ex5 = Obj [("x", Attached ex4), ("y", Attached ex3), ("z", Attached ex2)]

ex6 :: Term
ex6 = Dot (Dot (App t ("z", u)) "x") "y"
  where
    t =
      Obj
        [ ("x", Attached (Obj [("y", Attached (Dot (Loc 1) "z"))])),
          ("z", VoidAttr)
        ]
    u = Obj []

ex7 :: Term
ex7 = App t ("y", u)
  where
    t = Obj [("x", VoidAttr), ("y", VoidAttr), ("z", Attached (Loc 0))]
    u = App (Dot (Dot (Loc 1) "x") "a") ("y", (Dot (Loc 1) "a"))

ex8 :: Term
ex8 =
  Dot (Obj [("x", Attached $ App (Dot (Loc 3) "y") ("y", Dot (Loc 3) "z"))]) "x"

ex9 :: Term
ex9 = App (Dot t "d") ("b", Loc 0)
  where
    t =
      Obj
        [ ("a", VoidAttr),
          ("b", VoidAttr),
          ("c", Attached (Loc 2)),
          ("d", Attached (Obj [])),
          ("e", Attached (Loc 3))
        ]

ex10 :: Term
ex10 = Dot t "z"
  where
    t =
      Obj
        [ ("x", VoidAttr),
          ("y", VoidAttr),
          ("z", Attached (App (Loc 0) ("y", Loc 0)))
        ]

ex11 :: Term
ex11 = Dot (Obj [("z", Attached (Obj [("z", Attached (Loc 2))]))]) "z"

ex12 :: Term
ex12 =
  App
    (Dot (Obj [("x", Attached (Obj [])), ("z", Attached (Loc 0))]) "x")
    ("y", Loc 1)

ex13 :: Term
ex13 = App (Dot (Obj [("x", Attached (Obj []))]) "x") ("y", Loc 0)

ex14 :: Term
ex14 = Dot (Obj [("x", Attached (App t ("y", Loc 0)))]) "x"
  where
    t = Obj [("y", VoidAttr), ("x", Attached (Loc 2))]

ex15 :: Term
ex15 = Dot (Obj [("a", Attached (Dot ex14 "x"))]) "a"

ex16 :: Term
ex16 = Dot (Dot t "a") "y"
  where
    t =
      Obj
        [ ( "a",
            Attached
              ( Obj
                  [ ("z", Attached (Loc 1)),
                    ("y", Attached (Obj [("x", Attached (Loc 1))]))
                  ]
              )
          )
        ]

ex17 :: Term
ex17 = Dot (Dot (Obj [("x", Attached $ App t ("a", Loc 1))]) "x") "y"
  where
    t = Obj [("a", VoidAttr), ("y", Attached $ Obj [("z", Attached (Loc 1))])]

-- ⟦ v3 ↦ ø, a3 ↦ ρ⁰(v3 ↦ ρ¹.v2)(v3 ↦ ρ¹) ⟧.a3
ex18 :: Term
ex18 = Dot (Obj [("x", VoidAttr), ("y", Attached t)]) "y"
  where
    t = App (Loc 0) ("x", Loc 1)

ex19 :: Term
ex19 = Dot (Obj [("x", VoidAttr), ("y", Attached t)]) "y"
  where
    t = App (Loc 0) ("x", DataTerm (DataInteger 1))

-- * Terms translated from \(\lambda\)-calculus to \(\varphi\)-calculus

-- | Apply a term that represents \(\lambda\)-term to another term.
--
-- >>> app (Loc 0) (Loc 1)
-- ρ⁰(𝜑 ↦ ρ¹).r
app :: Term -> Term -> Term
app f x = Dot (App f ("𝜑", x)) "r"

-- | Wrap body of a \(\lambda\)-term into a translated \(\lambda\)-abstraction.
--
-- >>> identity = lam (Dot (Loc 0) "𝜑")
-- >>> identity
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧
-- >>> app identity identity
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧(𝜑 ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧).r
-- >>> nf (app identity identity )
-- ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧
lam :: Term -> Term
lam t = Obj [("𝜑", VoidAttr), ("r", Attached t)]

-- | Church numerals translated to \(\varphi\)-calculus.
--
-- >>> churchN 0
-- ⟦ 𝜑 ↦ ø, r ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ⁰.𝜑 ⟧ ⟧
-- >>> churchN 3
-- ⟦ 𝜑 ↦ ø, r ↦ ⟦ 𝜑 ↦ ø, r ↦ ρ¹.𝜑(𝜑 ↦ ρ¹.𝜑(𝜑 ↦ ρ¹.𝜑(𝜑 ↦ ρ⁰.𝜑).r).r).r ⟧ ⟧
churchN :: Int -> Term
churchN n = lam (lam (iterate (app s) z !! n))
  where
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Increment combinator for Church numerals.
--
-- >>> churchN 3 == nf (app churchInc (churchN 2))
-- True
churchInc :: Term
churchInc = lam (lam (lam (app s (app (app n s) z))))
  where
    n = Dot (Loc 2) "𝜑"
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Combinator for adding two Church numerals.
--
-- \(5 = 2 + 3\):
-- >>> churchN 5 == nf (app (app churchPlus (churchN 2)) (churchN 3))
-- True
churchPlus :: Term
churchPlus = lam (lam (lam (lam (app (app n s) (app (app m s) z)))))
  where
    n = Dot (Loc 3) "𝜑"
    m = Dot (Loc 2) "𝜑"
    s = Dot (Loc 1) "𝜑"
    z = Dot (Loc 0) "𝜑"

-- | Combinator for multiplying two Church numerals.
--
-- \(6 = 2 \times 3\):
-- >>> churchN 6 == nf (app (app churchMul (churchN 2)) (churchN 3))
-- True
churchMul :: Term
churchMul = lam (lam (lam (app n (app m s))))
  where
    n = Dot (Loc 2) "𝜑"
    m = Dot (Loc 1) "𝜑"
    s = Dot (Loc 0) "𝜑"

-- | Church-encoded exponentiation combinator.
--
-- \(8 = 2^3\):
-- >>> churchN 8 == nf (app (app churchExp (churchN 2)) (churchN 3))
-- True
churchExp :: Term
churchExp = lam (lam (app m n))
  where
    n = Dot (Loc 1) "𝜑"
    m = Dot (Loc 0) "𝜑"
