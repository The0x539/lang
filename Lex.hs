import Data.List
import Data.Maybe

(?:) :: a -> Maybe [a] -> Maybe [a]
x ?: Just y = Just (x:y)
x ?: Nothing = Nothing

scan :: (a -> Maybe (b, a)) -> a -> Maybe [b]
scan f input = case f input of
  Nothing -> Nothing
  Just (x, xs) -> x ?: (scan f xs)
