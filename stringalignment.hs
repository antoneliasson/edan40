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
    maximaBy score $ concat [attachHeads x y (optAlignments xs ys),
        attachHeads '-' y (optAlignments (x:xs) ys),
        attachHeads x '-' (optAlignments xs (y:ys))]
    where
    score :: AlignmentType -> Int
    score ([], []) = 0
    score (x:xs, y:ys) =
        sc x y + score (xs, ys)
        where
        sc x y
            | x == y = scoreMatch
            | x == '-' = scoreSpace
            | y == '-' = scoreSpace
            | otherwise = scoreMismatch

-- 2e (using the slow algorithm).

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments s1 s2 = do
    let as = optAlignments s1 s2
    putStrLn ("There are " ++ (show $ length as) ++ " optimal alignments:\n")
    putStrLn $ format as
    where
        format as = foldl1 (++) (map (\(x,y) -> x ++ "\n" ++ y ++ "\n\n") as)

-- 3.

fastSimilarityScore :: String -> String -> Int
fastSimilarityScore xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable :: [[Int]]
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry :: Int -> Int -> Int
    simEntry i 0 = i*scoreSpace
    simEntry 0 j = j*scoreSpace
    simEntry i j =
        maximum [simScore (i-1) (j-1) + score x y,
            simScore (i-1) (j) + scoreSpace,
            simScore i (j-1) + scoreSpace]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
         score x y
            | x == y    = scoreMatch
            | otherwise = scoreMismatch

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails t1 t2 aList = [(xs ++ [t1], ys ++ [t2]) | (xs, ys) <- aList]

fastOptAlignments :: String -> String -> [AlignmentType]
fastOptAlignments xs ys = snd $ optAlign (length xs) (length ys)
    where
    optAlign i j = optTable!!i!!j
    optTable :: [[(Int, [AlignmentType])]]
    optTable = [[ optEntry i j | j<-[0..]] | i<-[0..]]

    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = (scoreSpace + fst (optEntry (i-1) 0), attachTails (xs!!(i-1)) '-' $ snd (optEntry (i-1) 0))
    optEntry 0 j = (scoreSpace + fst (optEntry 0 (j-1)), attachTails '-' (ys!!(j-1)) $ snd (optEntry 0 (j-1)))
    optEntry i j = (fst (best!!0), concat $ map snd best)
        where
        best = maximaBy fst [diag, right, down]
            where
            diag, right, down :: (Int, [AlignmentType])
            diag
                | x == y    = addScoreAndTails scoreMatch x y (optAlign (i-1) (j-1))
                | otherwise = addScoreAndTails scoreMismatch x y (optAlign (i-1) (j-1))
            right = addScoreAndTails scoreSpace '-' y (optAlign i (j-1))
            down = addScoreAndTails scoreSpace x '-' (optAlign (i-1) j)
            x = xs!!(i-1)
            y = ys!!(j-1)

            addScoreAndTails :: Int -> Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
            addScoreAndTails sc a b (score, as) = (score + sc, attachTails a b as)

-- Output function using the fast algorithm

fastOutputOptAlignments :: String -> String -> IO ()
fastOutputOptAlignments s1 s2 = do
    let as = fastOptAlignments s1 s2
    putStrLn ("There are " ++ (show $ length as) ++ " optimal alignments:\n")
    putStrLn $ format as
    where
        format as = foldl1 (++) (map (\(x,y) -> x ++ "\n" ++ y ++ "\n\n") as)
