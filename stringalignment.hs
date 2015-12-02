module Main where

-- 1. An algorithm for the string alignment problem can be used to solve the
--    MCS for strings by setting scoreMismatch to infinite penalty. That way
--    only spaces will be used in the alignment solution. Every character that
--    is not a space in any of the resulting alignment strings is part of the
--    MCS

scoreMatch = 0 :: Int
scoreMismatch = -1 :: Int
scoreSpace = -1 :: Int
string1 = "writers"
string2 = "vintner"

-- 2a.

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (x:xs) [] = scoreSpace + similarityScore xs []
similarityScore [] (y:ys) = scoreSpace + similarityScore [] ys
similarityScore (x:xs) (y:ys) =
    max3 (similarityScore xs ys + score x y)
         (similarityScore xs (y:ys) + scoreSpace)
         (similarityScore (x:xs) ys + scoreSpace)
    where
        max3 x y = max $ max x y
        score x y
            | x == y    = scoreMatch
            | otherwise = scoreMismatch
