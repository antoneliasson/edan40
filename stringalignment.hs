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
    maximum [similarityScore xs ys + score x y,
        similarityScore xs (y:ys) + scoreSpace,
        similarityScore (x:xs) ys + scoreSpace]
    where
        score x y
            | x == y    = scoreMatch
            | otherwise = scoreMismatch

-- 2b.

-- For every tuple in the list in the third argument: Prepend the first
-- argument to the list in the first part of the tuple, and prepend the second
-- argument to the list in the second part of the tuple.
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

-- 2c.

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy f xs = [x | x <- xs, (maximum (map f xs)) == (f x)]

-- 2d.

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs []
optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) (y:ys) =
    concat $ maximaBy cmpAlignments [attachHeads x y $ optAlignments xs ys,
        attachHeads x '-' $ optAlignments xs (y:ys),
        attachHeads '-' y $ optAlignments (x:xs) ys]
    where
        cmpAlignments :: [AlignmentType] -> Int
        cmpAlignments ((x1, x2):xs) = similarityScore x1 x2
