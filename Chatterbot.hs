module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind brain = do
  r <- randomIO :: IO Float
  return (rulesApply (makeList r brain))
  where
    makeList :: Float -> BotBrain -> [PhrasePair]
    makeList r brain = map (\(q, as) -> (q, pick r as)) brain

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply pairs phrase = fromMaybe [] $ transformationsApply "*" reflect pairs phrase

reflect :: Phrase -> Phrase
reflect = map (try (\x -> lookup x reflections))

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile rules = map (map2 (words . (map toLower), map (words))) rules

--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute wildcard (t:ts) s
  | wildcard == t       = s ++ substitute wildcard ts s
  | otherwise           = t:substitute wildcard ts s


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] []           = Just []
match _ [] (s:ss)       = Nothing
match _ (p:ps) []       = Nothing
match wc (p:ps) (s:ss)
  | wc /= p             = if p == s then match wc ps ss else Nothing
  | otherwise           = singleWildcardMatch wc (p:ps) (s:ss) `orElse` longerWildcardMatch wc (p:ps) (s:ss)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => a -> [a] -> [a] -> Maybe [a]
singleWildcardMatch wc (p:ps) (s:ss) = if isJust (match wc ps ss) then Just [s] else Nothing
longerWildcardMatch wc (p:ps) (s:ss) = mmap ((:) s) (match wc (p:ps) ss)



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f l p = mmap (substitute wc (snd p)) (mmap f (match wc (fst p) l))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (p:ps) l = transformationApply wc f l p `orElse` transformationsApply wc f ps l
