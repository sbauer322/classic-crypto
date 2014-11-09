module Util where

import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Matrix as Matrix
import qualified Data.Maybe as Maybe

-- Returns the possible number of alphabets in use given a list of the repition lengths. Does not include 1 as a possible number.
repAnalysis :: [Int] -> [Int]
repAnalysis xs = tail $ foldl1 L.intersect $ L.nub . map allFactors $ map (\(x,y) -> gcd x y) $ pairs
    where pairs = [(x,y) | x <- xs, y <- xs, x /= y] 

alphabetSize :: Int
alphabetSize = 26

encoding :: String -> (Int -> Int) -> String
encoding p f = map (toChar . f . toDigit) (stripWhitespace p)

decoding :: String -> (Int -> Int) -> String
decoding c f = map (Char.toLower . toChar . f . toDigit) (stripWhitespace c)

-- A..Z, for results to make sense
toDigit :: Char -> Int
toDigit c = (Char.ord . Char.toUpper $ c) - 65

-- Must be 0..25 for results to make sense
toChar :: Int -> Char
toChar i = Char.toUpper . Char.chr $ (i + 65)

stripWhitespace :: String -> String
stripWhitespace = L.filter (/=' ')

printEncoded :: String -> String
printEncoded s 
    | L.length s > 5 = L.take 5 s ++ " " ++ printEncoded (L.drop 5 s)
    | otherwise    = L.take 5 s

isInverseMod :: Int -> Int -> Bool
isInverseMod a n = gcd a n == 1

inversesMod :: (Integral a) => M.Map a a
inversesMod = M.fromList [(1,1),(3,9),(5,21),(7,15),(9,3),(11,19),(15,7),(17,23),(19,11),(21,5),(23,17),(25,25)]

lookupInverseMod :: (Integral a) => a -> a
lookupInverseMod x 
    | M.lookup x inversesMod == Nothing = error "No matching inverse value found!"
    | otherwise = Maybe.fromJust (M.lookup x inversesMod)

-- Produces a list of tuples, each containing one character used and the number of times used in the provided string. Sorted from most common to least.
letterFrequency :: String -> [(Char, Int)]
letterFrequency s = reverse . L.sortBy sortLetterFrequency $ (map (\x -> (head x, length x)) sorted)
    where sorted = L.group . L.sort . stripWhitespace . map Char.toUpper $ s
          sortLetterFrequency (a,b) (c,d)
            | b < d = LT
            | b > d = GT
            | b == d = EQ

-- Not elegant, but works for now.
averageFrequency :: [(Char, Int)]
averageFrequency = frequencyAnalysis "aaaaaaaabccdddddeeeeeeeeeeeeffgghhhhhhhiiiiiiijklllllmmnnnnnnnoooooooopqrrrrrsssssstttttttttuuuvwwwxyyz"

-- Approximate breakdown of letter percentages, higher means used more. Out of 100, not in list means not used.
frequencyAnalysis :: String -> [(Char, Int)]
frequencyAnalysis s = map (\(x,y)->(x,round ((fromIntegral y/fromIntegral total)*100))) . letterFrequency $ s
    where total = L.length . stripWhitespace $ s

-- From the text's chart, all the digraphs with count at or above 100.
mostCommonDigraph :: [(Char,Char,Int)]
mostCommonDigraph = 
    [('a','i',117),('a','l',280),('a','n',359),('a','r',152),('a','s',235),('a','t',251),('b','e',118),('c','e',139),('d','a',111),('d','i',108),('d','o',168),('d','t',133),('e','a',260),('e','d',257),('e','e',111),('e','l',161),('e','m',111),('e','n',218),('e','r',444),('e','s',227),('e','t',211),('e','w',148),('h','a',267),('h','e',761),('h','i',195),('h','o',138),('i','c',131),('i','d',106),('i','n',450),('i','s',156),('i','t',321),('l','e',185),('l','i',208),('l','l',180),('m','e',138),('n','a',102),('n','d',337),('n','e',105),('n','g',262),('n','o',150),('n','t',186),('o','f',144),('o','n',235),('o','o',115),('o','r',174),('o','t',146),('o','u',355),('o','w',162),('r','a',119),('r','e',213),('r','i',126),('r','s',121),('r','y',100),('s','a',179),('s','e',217),('s','h',270),('s','o',123),('s','t',197),('t','a',158),('t','e',144),('t','h',725),('t','i',180),('t','o',293),('t','s',131),('t','t',176),('u','r',109),('u','s',125),('u','t',129),('v','e',149),('w','a',183),('w','h',104),('x','o',113)]

allFactors :: Int  -> [Int]
allFactors x = L.nub . L.sort . map product . L.subsequences $ primeFactors
    where primeFactors = concatMap (\(x,y) -> [x | _ <- [1..y]]) (M.toList . decomposeNumber $ x)

-- Given a non negative integer, returns a map where the keys are factors and the values are the factors' exponent.
decomposeNumber :: Int -> M.Map Int Int
decomposeNumber x = decompose x (sieve [2..]) M.empty

decompose :: Int -> [Int] -> M.Map Int Int -> M.Map Int Int
decompose x all@(y:ys) m | x < y = M.empty
                | x == y = M.insertWith (+) y 1 m
                | x `mod` y == 0 = decompose (x `div` y) all (M.insertWith (+) y 1 m)
                | otherwise = decompose (x) (ys) m

-- Modified from The Genuine Sieve of Eratosthenes by M. E. O'Neill.
sieve :: (Num b, Ord b) => [b] -> [b]
sieve xs = sieve' xs M.empty
   where
     sieve' []   table = []
     sieve' (x:xs) table =
       case M.lookup x table of
           Nothing -> x : sieve' xs (M.insert (x*x) [x] table)
           Just facts -> sieve' xs (Prelude.foldl reinsert (M.delete x table) facts)
         where
           reinsert table prime = M.insertWith (++) (x+prime) [prime] table

determinant :: (Integral a) => Matrix.Matrix a -> a
determinant m = (a * d) - (b * c)
    where a = Matrix.getElem 1 1 m
          b = Matrix.getElem 1 2 m
          c = Matrix.getElem 2 1 m
          d = Matrix.getElem 2 2 m

inverseModMatrix :: (Integral a) => a -> Matrix.Matrix a -> Matrix.Matrix a
inverseModMatrix x m = modMatrix x . Matrix.scaleMatrix modDeterminant $ (Matrix.fromList 2 2 [d, (-b), (-c), a])
    where modDeterminant = lookupInverseMod . (`mod` x) . determinant $ m
          a = Matrix.getElem 1 1 m
          b = Matrix.getElem 1 2 m
          c = Matrix.getElem 2 1 m
          d = Matrix.getElem 2 2 m

checkInverseMatrix :: (Integral a) => Matrix.Matrix a -> Matrix.Matrix a -> Matrix.Matrix a
checkInverseMatrix orig inv = Matrix.multStd orig inv

modMatrix :: (Integral a) => a -> Matrix.Matrix a -> Matrix.Matrix a
modMatrix x m = Matrix.fromList (Matrix.nrows m) (Matrix.ncols m) . map (`mod` x) . Matrix.toList $ m
