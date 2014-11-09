module VigenereSquare where

import qualified Data.List as List
import qualified Shift as Shift
import qualified Util as Util

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

keyElim :: String -> Int -> String
keyElim cipher shift = decode cipher shiftedCipher
    where shiftedCipher = 

