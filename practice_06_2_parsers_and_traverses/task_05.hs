{-# LANGUAGE InstanceSigs #-}

import Control.Applicative
import qualified Control.Arrow as Data.Bifunctor

newtype Parser a = Parser { apply :: String -> [(a, String)] }

parse :: Parser a -> String -> [a]
parse p = map fst . filter (null . snd) . apply p

instance Functor Parser where
    fmap f (Parser one) = Parser two
        where
            convert = map (Data.Bifunctor.first f)
            two s = convert (one s)

instance Applicative Parser where
    pure a = Parser (\s -> [(a, s)])
    (<*>) a b = Parser (\s -> [ (f x, sx) | (f, sf) <- apply a s, (x, sx) <- apply b sf])


instance Alternative Parser where
    empty = Parser (const [])
    (<|>) a b = Parser (\s -> apply a s ++ apply b s)
