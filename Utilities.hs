module Utilities where

-- Simultaneous map of two functions over the two parts of a tuple
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Apply a function to a Maybe type, if possible. Otherwise do nothing
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- If the first argument is a Just a, return it. Otherwise return the second argument.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Maybe apply a function to an object, otherwise pass it through
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Fix point iterate a function on an object until it no longer changes
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Take an element from a list indexed by a fraction of the list length
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
