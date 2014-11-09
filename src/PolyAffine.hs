module PolyAffine where

import qualified Data.List as L
import qualified Util as Util
import qualified Affine as Affine

encode :: String -> [(Int, Int)] -> String
encode p keys = concatMap formula $ pair
    where formula (x,(a,b)) = Affine.encode [x] a b
          pair = zip (Util.stripWhitespace p) . L.cycle $ keys

decode :: String -> [(Int, Int)] -> String
decode c keys = concatMap formula $ pair
    where formula (x,(a,b)) = Affine.decode [x] a b
          pair = zip (Util.stripWhitespace c) . L.cycle $ keys
