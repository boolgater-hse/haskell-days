module Easy where

import LambdaCalculus.Lib
import Parser

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Exception (assert)
import Data.Maybe ( isNothing )

infix 1 `alphaEq`, `betaEq`

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var a) (Var b) = a == b
alphaEq (a :@ b) (a' :@ b') = alphaEq a a' && alphaEq b b'
alphaEq (Lam var body) (Lam var1 body1)
    | var == var1 = alphaEq body body1
    | otherwise = alphaEq (subst var (Var temp) body) (subst var1 (Var temp) body1)
    where
        temp = fresh (free body `Set.union` free body1) "a"
alphaEq _ _ = False

betaEq :: Expr -> Expr -> Bool
betaEq a b = nf a `alphaEq` nf b

free :: Expr -> Set Symb
free (Var var) = Set.singleton var
free (e1 :@ e2) = free e1 `Set.union` free e2
free (Lam var body) = var `Set.delete` free body

fresh :: Set Symb -> Symb -> Symb
fresh conflicts what
    | what `Set.member` conflicts = fresh conflicts (what ++ "'")
    | otherwise = what

alpha :: Set Symb -> Expr -> Expr
alpha conflicts var@(Var _) = var
alpha conflicts (e1 :@ e2) = alpha conflicts e1 :@ alpha conflicts e2
alpha conflicts (Lam var body)
    | var `Set.member` conflicts = renamed_lam
    | otherwise = alpha_lam
    where
        alpha_lam = Lam var (alpha conflicts body)
        renamed_lam = Lam new_var new_body
        new_var = fresh (conflicts `Set.union` free body) var
        new_body = subst var (Var new_var) body

subst :: Symb -> Expr -> Expr -> Expr
subst instead what (Var var)
    | var == instead = what
    | otherwise = Var var
subst instead what (e1 :@ e2) = subst instead what e1 :@ subst instead what e2
subst instead what (Lam var body)
    | var == instead = Lam var body
    | otherwise = new_lam
    where
        alpha_lam = alpha (free what) (Lam var body)
        new_body = subst instead what body
        new_lam
            | var `Set.member` free what = subst instead what alpha_lam
            | otherwise = Lam var new_body

normal :: Expr -> Bool
normal (Var _) = True
normal ((Lam _ _) :@ _) = False
normal (e1 :@ e2) = normal e1 && normal e2
normal (Lam _ body) = normal body

reduceOnce :: Expr -> Maybe Expr
reduceOnce expr
    | normal expr = Nothing
    | otherwise = Just reduced
    where
        helper :: Expr -> Expr
        helper var@(Var _) = var
        helper ((Lam var body) :@ e2) = subst var e2 body
        helper (e1 :@ e2)
            | normal e1 = e1 :@ helper e2
            | otherwise = helper e1 :@ e2
        helper (Lam var body) = Lam var (helper body)
        reduced = helper expr

reduceOnce' :: Expr -> Expr
reduceOnce' var@(Var _) = var
reduceOnce' ((Lam var body) :@ e2) = subst var e2 body
reduceOnce' (e1 :@ e2)
    | normal e1 = e1 :@ reduceOnce' e2
    | otherwise = reduceOnce' e1 :@ e2
reduceOnce' (Lam var body) = Lam var (reduceOnce' body)

reduceLeft :: Expr -> [Expr]
reduceLeft = iterate reduceOnce'

nf :: Expr -> Expr
nf expr = head (dropWhile (not . normal) (reduceLeft expr))

main :: IO ()
main = do
    assert (subst "y" (Var "x") (Lam "x'" $ Var "x'" :@ Var "y") == Lam "x'" (Var "x'" :@ Var "x")) putStrLn "subst_ok1"

    assert (Lam "x" (Lam "y" (Var "x")) `alphaEq` Lam "y" (Lam "x" (Var "y"))) putStrLn "alphaeq_ok1"

    assert (isNothing (reduceOnce $ Lam "x" $ Lam "y" $ Var "x")) putStrLn "reduceone_ok1"
    assert (reduceOnce (Lam "x" $ Lam "y" $ Lam "z" (Var "z") :@ Var "x") == Just (Lam "x" (Lam "y" (Var "x")))) putStrLn "reduceone_ok2"
    assert (reduceOnce (Lam "x" (Var "x" :@ Var "x") :@ Lam "x" (Var "x" :@ Var "x")) == Just (Lam "x" (Var "x" :@ Var "x") :@ Lam "x" (Var "x" :@ Var "x"))) putStrLn "reduceone_ok3"

    assert (nf (fac :@ three) `alphaEq` six) putStrLn "nf_ok1"

    assert (fac :@ three `betaEq` six) putStrLn "betaeq_ok1"

    -- Lam "f" (Lam "x" (Var "f" :@ (Var "x" :@ Var "x")) :@ Lam "x" (Var "f" :@ (Var "x" :@ Var "x")))
    let cY = let {x = Var "x"; f = Var "f"; fxx = Lam "x" $ f :@ (x :@ x)} in Lam "f" $ fxx :@ fxx
    assert (show (Lam "x" (Var "x" :@ Var "y")) == "\\x -> x y") putStrLn "show_ok1"
    assert (show cY == "\\f -> (\\x -> f (x x)) (\\x -> f (x x))") putStrLn "show_ok2"
    assert ((read "\\x1 x2 -> x1 x2 x2" :: Expr) == Lam "x1" (Lam "x2" (Var "x1" :@ Var "x2" :@ Var "x2"))) putStrLn "read_ok1"
    assert (read (show cY) == cY) putStrLn "read_ok2"
