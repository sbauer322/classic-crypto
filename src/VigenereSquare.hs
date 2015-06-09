module VigenereSquare where

import qualified Data.List as List
import qualified Shift as Shift
import qualified Util as Util

-- When trying to break a possibly polyalphabetic cipher, don't forget to use repition analysis found in Util... it should give you the number of alphabets used.

-- When trying to determine if Vigenere, first determine the number of alphabets via the gcd of repetition analysis, then look at the frequency analysis to see if each alphabet looks like a normal alphabet, this would mean it is shifted. Thus likely a Vigenere.


-- Maps the Shift encode over each character.
encode :: String -> String -> String
encode p key = concat . map formula $ pair
    where formula (x,y) = Shift.encode [x] y
          pair = zip (Util.stripWhitespace p) . List.cycle . map Util.toDigit . Util.stripWhitespace $ key

-- Maps the Shift decode over each character.
decode :: String -> String -> String
decode c key = concat . map formula $ pair
    where formula (x,y) = Shift.decode [x] y
          pair = zip (Util.stripWhitespace c) . List.cycle . map Util.toDigit . Util.stripWhitespace $ key

--keyElim :: String -> Int -> String
--keyElim cipher shift = decode cipher shiftedCipher
--    where shiftedCipher = 

