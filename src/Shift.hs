module Shift where

import qualified Util as U

encode :: String -> Int -> String
encode p shift = U.encoding p $ (`mod` U.alphabetSize) . (+ shift)

decode :: String -> Int -> String
decode c shift = U.decoding c $ (`mod` U.alphabetSize) . (+) (-shift)

completeTheAlphabet :: String -> [(String, Int)]
completeTheAlphabet c = map (decode' c) [0..U.alphabetSize-1]
    where decode' c shift = (decode c shift, shift)

caesar :: String -> String
caesar p = encode p 3

decaesar :: String -> String
decaesar c = decode c 3
