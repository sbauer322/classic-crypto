module Affine where

import qualified Util as U

encode :: String -> Int -> Int -> String
encode p a b = U.encoding p formula
    where formula x = (`mod` U.alphabetSize) ((a*x)+b)

-- Solving for P, given ciphertext c.
decode :: String -> Int -> Int -> String
decode c aBar b = U.decoding c formula
    where formula x = (`mod` U.alphabetSize) (U.lookupInverseMod aBar * (x-b))

-- First parameter in each tuple is the coefficient of a.
-- The coefficient of a is the plaintext letter.
--
-- TODO: If there is a no matching inverse value found error, try swapping the tuples and excuting again before giving up.
cryptanalysis :: (Int, Int) -> (Int, Int) -> (Int, Int)
cryptanalysis (c1, p1) (c2, p2) = let 
    solveA = ((-c1)+c2, (-p1)+p2)
    modA = U.lookupInverseMod (fst solveA)
    a = modA * (snd solveA) `mod` 26
    
    solveB = ((c1*a), p1)
    modB = (fst solveB) `mod` 26
    b = ((snd solveB)-modB) `mod` 26

    in (a,b)

cryptanalysisChar :: (Char, Char) -> (Char, Char) -> (Int, Int)
cryptanalysisChar (c1, p1) (c2, p2) = cryptanalysis (convert c1, convert p1) (convert c2, convert p2)
    where convert x = U.toDigit x

-- TODO: Function to automate the initial cryptanalysis to try matching the top 3 most frequent cipher letters to the top 3 most frequent english letters e,t,a
