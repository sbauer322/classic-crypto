module HillDigraph where

import qualified Data.List as List
import qualified Data.Matrix as Matrix
import qualified Util as Util

;;encode :: (Integral a) => String ->  (a, a, a, a) -> String
;;encode p key = 
;;    where paired = map (\

;; Hot and loose encoding of pairs
;; map Util.toChar (Matrix.toList (encodeHelper ((\[a, b] -> (a,b)) (map Util.toDigit "od")) (11,9,19,10)))



encodeHelper :: (Integral a) => (a, a) ->  (a, a, a, a) ->  Matrix.Matrix a
encodeHelper (p1, p2) (a1, b1, c1, d1) = modM
    where mP = Matrix.fromList 2 1 [p1, p2]
          mE = Matrix.fromList 2 2 [a1,b1,c1,d1]
          modM = Util.modMatrix 26 (Matrix.multStd mE mP)
