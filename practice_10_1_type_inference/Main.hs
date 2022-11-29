{-# LANGUAGE FlexibleContexts #-}
import SST.Lib
import Data.List
import Control.Monad.Except
import Data.Char
import Control.Monad.State

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
    mappend = composeSubsTy
    mempty = SubsTy []

freeVars :: Expr -> [Symb]
freeVars (Var a) = [a]
freeVars (e1 :@ e2) = freeVars e1 `union` freeVars e2
freeVars (Lam var body) = filter (var /=) (freeVars body)

freeTVars :: Type -> [Symb]
freeTVars (TVar a) = [a]
freeTVars (t1 :-> t2) = freeTVars t1 ++ freeTVars t2

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env e) a t = Env (e ++ [(a, t)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env e) = foldr (\a b -> b `union` freeTVars (snd a)) [] e

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
    Just t -> pure t
    Nothing -> throwError ("There is no variable " <> show v <> " in the environment.")

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy list) (TVar symbol) = case lookup symbol list of
    Just sm -> sm
    Nothing -> TVar symbol
appSubsTy subs (a :-> b) = appSubsTy subs a :-> appSubsTy subs b

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv _ (Env []) = Env []
appSubsEnv s q@(Env (e:env)) = extendEnv (appSubsEnv s (Env env)) sum (appSubsTy s t)
    where
        (sum, t) = e

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy a b = SubsTy (sub1 ++ helper2 a sub1)
    where
        sub1 = helper1 b a

        helper1 :: SubsTy -> SubsTy -> [(Symb, Type)]
        helper1 (SubsTy a) subs = map (\x -> (fst x, appSubsTy subs (snd x))) a

        helper2 :: SubsTy -> [(Symb, Type)] -> [(Symb, Type)]
        helper2 (SubsTy a) b = filter (\x -> not (isIn b (fst x)) ) a

        isIn :: [(Symb, Type)] -> Symb -> Bool
        isIn [] a = False
        isIn (x:xs) a = (a == fst x) || isIn xs a

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar a) (TVar b)
    | a == b = return (SubsTy [])
    | otherwise = return (SubsTy [(a , TVar b)])
unify (TVar a) t
    | a `elem` freeTVars t = throwError ("Can't unify (TVar "++ show a ++ ") with (" ++ show t ++ ")!")
    | otherwise = return (SubsTy [(a, t)])
unify (s1 :-> s2) (TVar a) = unify (TVar a) (s1 :-> s2)
unify (s1 :-> s2) (t1 :-> t2) =
    case unify s2 t2 of
        Left err -> throwError err
        Right s -> do
            case unify (appSubsTy s s1) (appSubsTy s t1) of
                Left err -> throwError err
                Right s' -> return (composeSubsTy s' s)

getFreshSym :: Integer -> Symb
getFreshSym = show

genStartingEnv :: Expr -> Env
genStartingEnv ex = Env (helper (freeVars ex) 1)
    where
        helper (x:xs) n = (x, TVar (getFreshSym n)) : helper xs (n+1)
        helper [] n = []

principlePair :: (MonadError String m) => Expr -> m (Env,Type)
principlePair ex = do
    let startEnv = genStartingEnv ex
    equations <- equations startEnv ex (TVar "0")
    let (l, r) = foldr1 (\(l, r) (l', r') -> (l :-> l', r :-> r')) equations
    sbt <- unify l r
    return (appSubsEnv sbt startEnv, appSubsTy sbt (TVar "0"))

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type,Type)]
equations env x t = evalStateT (helper env x t) 0
    where
        helper env (Var x) t = do
            xType <- lift (appEnv env x)
            return [(t, xType)]
        helper env (l :@ r) t = do
            st <- get
            put (st + 1)
            let tvar = TVar (show st ++ "'''")
            l <- helper env l (tvar :-> t)
            r <- helper env r tvar
            return (l ++ r)
        helper env (Lam x exp) t = do
            st <- get
            put (st + 1)
            st2 <- get
            put (st2 + 1)
            let tvar1 =  TVar (show st ++ "'''")
            let tvar2 = TVar (show st2 ++ "'''")
            l <- helper (extendEnv env x tvar1) exp tvar2
            return ((tvar1 :-> tvar2, t) : l)
