module Lex where

type Parse a b = a -> Maybe (b, a)

infixr 3 &>
(&>) :: Parse a [b] -> Parse a [b] -> Parse a [b]
(f &> g) x = case f x of
  Nothing -> Nothing
  Just (y, ys) -> case g ys of
    Nothing -> Nothing
    Just (z, zs) -> Just (y ++ z, zs)

infixr 2 |>
(|>) :: Parse a b -> Parse a b -> Parse a b
(f |> g) x = case f x of
  Just y -> Just y
  Nothing -> g x

epsilon x = Just ([], x)
optional f = f |> epsilon

lexer :: Parse a [b] -> a -> Maybe [b]
lexer f input = case f input of
  Nothing -> Nothing
  Just (xs, rem) -> case lexer f rem of
    Nothing -> Nothing
    Just ys -> Just (xs ++ ys)
