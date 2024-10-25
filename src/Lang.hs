{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Lang (Name, Expr (Ap, Lam, Var), expr, subst, simp, frees, prepare) where

import Control.Spoon (spoon)
import Data.List (nub, sortOn)
import Data.String (IsString (fromString))
import Data.Text (Text, append, isSuffixOf, pack, unpack)
import Logging
import Numeric.Natural (Natural)
import Prelude hiding (log)

type Log = Logged String
type Name = Text
data Expr = Ap Expr Expr | Lam Name Expr | Var Name

infixr 9 `Lam`
infixl 9 `Ap`

wrap :: String -> String
wrap = ("(" ++) . (++ ")")

toNat :: Expr -> Maybe Natural
toNat (s `Lam` z `Lam` n) = tryNat n
  where
    tryNat :: Expr -> Maybe Natural
    tryNat (Var s' `Ap` x) | s' == s = (1 +) <$> tryNat x
    tryNat (Var z') | z' == z = Just 0
    tryNat _ = Nothing
toNat _ = Nothing

instance Show Expr where
    show (toNat -> Just n) = show n
    show (Var n) = unpack n
    show (Lam n e) = wrap $ "\\" ++ unpack n ++ ", " ++ show e
    show (Ap a b@(Ap _ _)) = show a ++ " " ++ wrap (show b)
    show (Ap a b) = show a ++ " " ++ show b

instance IsString Expr where
    fromString = Var . pack

indent :: String -> String
indent = ('\t' :)

-- | A function to help with processing 'Expr's
expr :: (b -> b -> b) -> (Name -> b -> b) -> (Name -> b) -> Expr -> b
expr ap lam var = rec
  where
    rec (Ap f x) = ap (rec f) (rec x)
    rec (Lam n e) = lam n (rec e)
    rec (Var n) = var n

rescheme :: (Name -> Name) -> Expr -> Expr
rescheme rn = expr Ap (\n e -> let n' = rn n in Lam n' $ forget $ subst n (Var n') e) Var

takes :: Int -> [a] -> [[a]]
takes 0 _ = [[]]
takes n as = do
    a <- as
    l <- takes (n - 1) as
    return (a : l)

suffixes :: [Name]
suffixes =
    pack <$> do
        n <- [1 ..]
        sortOn (length . nub) $ takes n "_-'"

safeSuffix :: [Name] -> Name
safeSuffix lst = head $ dropWhile (\suf -> any (isSuffixOf suf) lst) suffixes
postpend :: Name -> Name -> Name
postpend suf n = append n suf

-- | Substitution
subst :: Name -> Expr -> Expr -> Log Expr
subst n v l@(Lam x e)
    | x == n = l <$ log ("stopped at " ++ show l)
    | elem x (frees v) =
        let suf = safeSuffix (frees v)
         in subst n v (rescheme (postpend suf) l)
    | otherwise = do
        log ("descended into " ++ show l)
        res <- region indent $ Lam x <$> (subst n v e)
        log ("resulted in " ++ show res)
        return res
subst n v a@(Ap f x) = do
    log ("recursing into " ++ show a)
    log ("first, " ++ show f)
    f' <- region indent $ subst n v f
    log ("second, " ++ show x)
    x' <- region indent $ subst n v x
    let a' = Ap f' x'
    log ("result: " ++ show a')
    return a'
subst n v (Var x)
    | x == n = v <$ log ("replaced var [" ++ unpack n ++ "] with val " ++ show v)
    | otherwise = Var x <$ log ("ignored var [" ++ unpack x ++ "]")

-- | checks if a name occurs free in an expression
free :: Name -> Expr -> Bool
free n = expr (||) (\m b -> if n == m then False else b) (== n)

-- | Combined η & β reduction
simp, once :: Expr -> Log Expr
once ex@(Ap (Lam n e) x) = do
    log $ "found β-reduction at " ++ show ex
    res <- region indent $ subst n x e
    log $ "β-reduced to " ++ show res
    simp res
once (Lam n (Ap f (Var m))) | n == m && not (free n f) = simp f
once x = x <$ log "done simplifying"
simp ex@(Ap (Lam n e) x) = do
    log $ "found β-reduction at " ++ show ex
    res <- region indent $ subst n x e
    log $ "β-reduced to " ++ show res
    simp res
simp (Lam n (Ap f (Var m))) | n == m && not (free n f) = simp f
simp (Ap f x) = do
    log $ "recursing into " ++ show f
    f' <- region indent $ simp f
    log $ "recursing into " ++ show x
    x' <- region indent $ simp x
    log $ "result of ap:" ++ show (Ap f x)
    once (Ap f' x')
simp l@(Lam n x) = do
    log $ "recursing into " ++ show l
    x' <- region indent $ simp x
    let l' = Lam n x'
    log $ "result: " ++ show l'
    once l'
simp (Var n) = Var n <$ log "ignored variable"

-- | List of free variables
frees :: Expr -> [Name]
frees = expr (++) (filter . (/=)) (: [])

lambdize :: Integer -> Expr
lambdize = Lam "s" . Lam "z" . helper
  where
    helper 0 = Var "z"
    helper n = Ap (Var "s") (helper (n - 1))

preps :: Name -> [Expr -> Expr]
preps n@(spoon . (read :: String -> Integer) . unpack -> Just k) = [forget . subst n (lambdize k)]
preps "+" = [forget . (subst "+" $ "a" `Lam` "b" `Lam` "s" `Lam` "z" `Lam` ("a" `Ap` "s" `Ap` ("b" `Ap` "s" `Ap` "z")))]
preps "*" = [forget . (subst "*" $ "a" `Lam` "b" `Lam` "s" `Lam` ("a" `Ap` ("b" `Ap` "s")))]
preps "^" = [forget . (subst "^" $ "a" `Lam` "b" `Lam` "s" `Lam` "z" `Lam` ("b" `Ap` "a" `Ap` "s" `Ap` "z"))]
preps _ = []

prepare :: Expr -> Expr
prepare ex = foldr id ex (frees ex >>= preps)
