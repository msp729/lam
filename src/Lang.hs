module Lang (Name, Expr (Ap, Lam, Var), expr, alpha, subst, beta, eta, simp) where

import Data.Text (Text, unpack)

type Name = Text
data Expr = Ap Expr Expr | Lam Name Expr | Var Name

instance Show Expr where
    show =
        expr
            (\a b -> a ++ " " ++ b)
            (\x e -> "\\" ++ unpack x ++ ", (" ++ e ++ ")")
            unpack

-- | A function to help with processing 'Expr's
expr :: (b -> b -> b) -> (Name -> b -> b) -> (Name -> b) -> Expr -> b
expr ap lam var = rec
  where
    rec (Ap f x) = ap (rec f) (rec x)
    rec (Lam n e) = lam n (rec e)
    rec (Var n) = var n

{- | α-conversion, sorta

it is *very* important for the renamer to be injective
-}
alpha :: (Name -> Name) -> Expr -> Expr
alpha rn = expr Ap (Lam . rn) (Var . rn)

-- | Substitution
subst :: Name -> Expr -> Expr -> Expr
subst n v (Lam x e)
    | x == n = Lam x e
    | otherwise = Lam x (subst n v e)
subst n v (Ap f x) = Ap (subst n v f) (subst n v x)
subst n v (Var x)
    | x == n = v
    | otherwise = Var x

-- | β-reduces
beta :: Expr -> Expr
beta (Ap (Lam n e) x) = beta $ subst n x e
beta (Ap f x) = Ap (beta f) (beta x)
beta (Lam n e) = Lam n (beta e)
beta (Var n) = Var n

-- | checks if a name occurs free in an expression
free :: Name -> Expr -> Bool
free n = expr (||) (\m -> if n == m then const False else id) (== n)

-- | η-reduces
eta :: Expr -> Expr
eta (Lam n (Ap f (Var m)))
    | n == m && not (free n f) = eta f
eta (Lam x e) = Lam x (eta e)
eta (Ap f x) = Ap (eta f) (eta x)
eta (Var n) = Var n

-- | Combined η & β reduction
simp, once :: Expr -> Expr
once (Ap (Lam n e) x) = simp $ subst n x e
once (Lam n (Ap f (Var m))) | n == m && not (free n f) = simp f
once x = x
simp (Ap (Lam n e) x) = simp $ subst n x e
simp (Lam n (Ap f (Var m))) | n == m && not (free n f) = simp f
simp (Ap f x) = once $ Ap (simp f) (simp x)
simp (Lam n x) = once $ Lam n (simp x)
simp (Var n) = once $ Var n
