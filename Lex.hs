import Data.List
import Data.Maybe

(?:) :: a -> Maybe [a] -> Maybe [a]
x ?: Just y = Just (x:y)
x ?: Nothing = Nothing

consume :: Eq a => ([a] -> Maybe [a]) -> ([a] -> b) -> [a] -> Maybe (b, [a])
consume f g input = case f input of
  Nothing -> Nothing
  Just x -> case stripPrefix x input of
    Nothing -> Nothing
    Just rem -> Just (g x, rem)


scan :: (a -> Maybe (b, a)) -> a -> Maybe [b]
scan f input = case f input of
  Nothing -> Nothing
  Just (x, xs) -> x ?: (scan f xs)
