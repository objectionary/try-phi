module PhiCalculus where

type Ident = String

data Term
  = Var Ident
  | Lam Ident Term
  | App Term Term
  deriving (Eq)

instance Show Term where
  show = ppTerm

substitute :: Term -> Ident -> Term -> Term
substitute t x (Var y)
  | x == y = t
  | otherwise = Var y
substitute t x (App t1 t2) =
  App (substitute t x t1) (substitute t x t2)
substitute t x (Lam y body)
  | x == y = Lam y body
  | otherwise = Lam y (substitute t x body)

normalize :: Term -> Term
normalize (App t1 t2) =
  case normalize t1 of
    Lam x body -> normalize (substitute t2 x body)
    t1'        -> App t1' (normalize t2)
normalize (Lam x body) = Lam x (normalize body)
normalize (Var x) = Var x

ppTerm :: Term -> String
ppTerm (Var x)      = x
ppTerm (Lam x body) = "\\" ++ x ++ "." ++ ppTerm body
ppTerm (App t1 t2)
  = "(" ++ ppTerm t1 ++ ") (" ++ ppTerm t2 ++ ")"

-- \x.x
ex1 :: Term
ex1 = Lam "x" (Var "x")

-- \x.\y.x(y)
ex2 :: Term
ex2 = Lam "x" (Lam "y" (App (Var "x") (Var "y")))

zero :: Term
zero = Lam "s" (Lam "z" (Var "z"))

nat :: Int -> Term
nat n = Lam "s" (Lam "z" (iterate succ (Var "z") !! n))
  where
    succ t = App (Var "s") t

-- | 2+2=4 !
--
-- >>> normalize (App (App plus (nat 2)) (nat 2))
-- \s.\z.(s) ((s) ((s) ((s) (z))))
plus :: Term
plus = Lam "n" (Lam "m"
  (Lam "s" (Lam "z" (App (App (Var "n") (Var "s")) (App (App (Var "m") (Var "s")) (Var "z"))))))
