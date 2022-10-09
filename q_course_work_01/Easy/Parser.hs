module Parser where

import LambdaCalculus.Lib

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

instance Show Expr where
    show (Var var) = var
    show (a@(e1 :@ e2) :@ Var var) = "(" ++ show a ++ ") " ++ var
    show (Var var :@ b@(e1 :@ e2)) = var ++ " (" ++ show b ++ ")"
    show (a@(Lam var body) :@ b@(Lam var1 body1)) = "(" ++ show a ++ ")" ++ " " ++ "(" ++ show b ++ ")"
    show (a@(Lam var body) :@ e2) = "(" ++ show a ++ ")" ++ " " ++ show e2
    show (e1 :@ b@(Lam var body)) = show e1 ++ " " ++ "(" ++ show b ++ ")"
    show (e1 :@ e2) = show e1 ++ " " ++ show e2
    show (Lam var body) = "\\" ++ var ++ " -> " ++ show body

instance Read Expr where
    readsPrec _ input = case parseTerm input of
        Left err -> error (show err)
        Right expr -> [(expr, "")]
        where
            spaced :: Parser a -> Parser a
            spaced x = spaces *> x <* spaces

            arrow :: Parser ()
            arrow = void (spaced (string "->"))

            lambda :: Parser ()
            lambda = void (char '\\')

            parentheses :: Parser a -> Parser a
            parentheses x = spaced (char '(') *> x <* spaced (char ')')

            var :: Parser String
            var = (:) <$> oneOf ['a'..'z'] <*> many (letter <|> digit <|> char '\'')
                where
                    oneOf = choice . (char <$>)

            term :: Parser Expr
            term = foldl1 (:@) <$> many1 (spaced expr)

            expr :: Parser Expr
            expr = parentheses term <|> variable <|> expression

            variable :: Parser Expr
            variable = Var <$> var

            expression :: Parser Expr
            expression = do
                lambda
                vars <- many1 (spaced var)
                arrow
                expr <- term
                pure (foldr Lam expr vars)

            parseTerm :: String -> Either ParseError Expr
            parseTerm = parse term "Error while parsing. Example: \"\\x1 x2 -> x1 (x2 x2)\""
