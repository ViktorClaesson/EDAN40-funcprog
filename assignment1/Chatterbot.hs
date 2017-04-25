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
  return (rulesApply ((map (map2 (id, pick r))) brain))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply pairs input = fromJust $ orElse (transformationsApply "*" reflect pairs input) (Just [])

reflect :: Phrase -> Phrase
reflect [] = []
reflect (x:xs) = replace reflections x : reflect xs

replace :: [(String, String)] -> String -> String
replace [] word = word
replace (pair:pairs) word = 
    if (word == (fst pair)) then snd pair 
    else if (word == (snd pair)) then fst pair
    else replace pairs word

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
rulesCompile [] = []
rulesCompile ((f, s):pairs) = (words (map toLower f), map words s) : rulesCompile pairs

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
reductionsApply = fix . try . (transformationsApply "*" id)

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc [] _ = []
substitute wc (p:pn) ls = if(p == wc) then ls ++ substitute wc pn ls else p:substitute wc pn ls

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wc (p:ps) (s:ls)
    | p == wc = orElse (singleWildcardMatch (p:ps) (s:ls)) (longerWildcardMatch (p:ps) (s:ls)) 
    | p == s = match wc ps ls
    | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
longerWildcardMatch (wc:ps) (x:xs) = mmap ([x] ++)    (match wc (wc:ps) xs)

-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f input (org, trans) = mmap (substitute wc trans) (mmap f (match wc org input))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f [] input = Nothing
transformationsApply wc f (pair:pairs) input = orElse (transformationApply wc f input pair) (transformationsApply wc f pairs input)