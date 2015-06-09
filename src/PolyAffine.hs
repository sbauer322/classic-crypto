module PolyAffine where

import qualified Data.List as L
import qualified Util as Util
import qualified Affine as Affine

-- When trying to break a possibly polyalphabetic cipher, don't forget to use repition analysis found in Util... it should give you the number of alphabets used.

encode :: String -> [(Int, Int)] -> String
encode p keys = concatMap formula $ pair
    where formula (x,(a,b)) = Affine.encode [x] a b
          pair = zip (Util.stripWhitespace p) . L.cycle $ keys

--  decode "wwslo" [(17,17),(7,2),(19,2)]
decode :: String -> [(Int, Int)] -> String
decode c keys = concatMap formula $ pair
    where formula (x,(a,b)) = Affine.decode [x] a b
          pair = zip (Util.stripWhitespace c) . L.cycle $ keys
