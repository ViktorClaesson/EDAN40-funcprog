scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
s1 = "writers"
s2 = "vintner"

score :: Char -> Char -> Int
score a b | a == '-' || b == '-' = scoreSpace
          | a == b               = scoreMatch
          | otherwise            = scoreMismatch

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy _ [x] = [x]
maximaBy f (x:xs) | currMax == prevMax = (x : prev)
                  | currMax > prevMax = [x]
                  | otherwise = prev
                      where prev = (maximaBy f xs)
                            prevMax = (f $ head prev)
                            currMax = (f x)

reverseTouple :: (String, String) -> (String, String)
reverseTouple (f, s) = (reverse f, reverse s)

similarityScore :: String -> String -> Int
similarityScore xs ys = sim (length xs) (length ys)
  where
    sim i j = table!!i!!j
    table = [[ entry i j | j<-[0..]] | i<-[0..] ]
       
    entry :: Int -> Int -> Int
    entry i 0 = i * scoreSpace
    entry 0 j = j * scoreSpace
    entry i j = maximum [(sim (i - 1) (j - 1)) + (score x y), (sim (i - 1) j) + (score x '-'), (sim i (j - 1)) + (score '-' y)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlignments :: String -> String -> [(String, String)]
optAlignments xs ys = map reverseTouple $ snd $ align (length xs) (length ys)
  where
    align i j = table!!i!!j
    table = [[entry i j | j<-[0..]] | i<-[0..]]
       
    entry :: Int -> Int -> (Int, [(String, String)])
    entry i 0 = (scoreSpace * i, [(concat $ take i $ repeat "-", "")])
    entry 0 j = (scoreSpace * j, [("", concat $ take j $ repeat "-")])
    entry i j = (high, agns)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
         aij  = align (i - 1) (j - 1)
         ai   = align (i - 1) j
         aj   = align i (j - 1)
         maxb = maximaBy fst [(fst aij + (score  x  y ), attachHeads  x  y  (snd aij)), 
                              (fst ai  + (score  x '-'), attachHeads  x '-' (snd ai )), 
                              (fst aj  + (score '-' y ), attachHeads '-' y  (snd aj ))]
         high = fst $ maxb!!0
         agns = concat $ map snd maxb

outputOptAlignments s1 s2 = putStrLn $ "There are " ++ len ++ " alignments: \n\n" ++ (output alignments)
                                where
                                    output ((f, s):[]) = (f ++ "\n" ++ s ++ "\n")
                                    output ((f, s):ts) = (f ++ "\n" ++ s ++ "\n\n") ++ (output ts)
                                    alignments = optAlignments s1 s2
                                    len = show $ length alignments