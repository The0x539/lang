import Data.List
import Data.Maybe

(?:) :: a -> Maybe [a] -> Maybe [a]
x ?: Just y = Just (x:y)
x ?: Nothing = Nothing

make :: Eq a => ([a] -> Maybe [a]) -> ([a] -> Maybe b) -> [a] -> Maybe (b, [a])
make f g input = case f input of
  Nothing -> Nothing
  Just x -> case g x of
    Nothing -> Nothing
    Just y -> case (stripPrefix x input) of
      Nothing -> Nothing
      Just rem -> Just (y, rem)


scan :: (a -> Maybe (b, a)) -> a -> Maybe [b]
scan f input = case f input of
  Nothing -> Nothing
  Just (x, xs) -> x ?: (scan f xs)
