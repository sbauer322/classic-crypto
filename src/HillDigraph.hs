module HillDigraph where

import qualified Data.List as List
import qualified Data.Matrix as Matrix
import qualified Util as Util

encode :: (Int, Int, Int, Int) -> String -> String
encode mE p = map Util.toChar (concatMap (Matrix.toList . encodeHelper mE) (Util.pairToInt p))

encodeHelper :: (Integral a) => (a, a, a, a) -> (a, a) ->  Matrix.Matrix a
encodeHelper (a1, b1, c1, d1) (p1, p2) = modM
    where mP = Matrix.fromList 2 1 [p1, p2]
          mE = Matrix.fromList 2 2 [a1,b1,c1,d1]
          modM = Util.modMatrix 26 (Matrix.multStd mE mP)

decode :: (Int, Int, Int, Int) -> String -> String
decode (a,b,c,d) cipher = encode decryptionMatrix cipher
    where decryptionMatrix = toTuple (Matrix.toList (Util.inverseModMatrix 26 (Matrix.fromList 2 2 [a,b,c,d])))
          toTuple [x1,x2,x3,x4] = (x1,x2,x3,x4)

-- Returns the possible **encryption** matrix based on the given inputs.
-- The format for inputs is as follows: 
--
-- First suspected ciphertext to plaintext mapping.
-- Second suspected ciphertext to plaintext mapping.
-- e.g., Util.modMatrix 26 (cryptanalysis (('z','l'),('h','e')) (('d','q'),('t','h')))
-- Where zl, dq are ciphertext and he, th are plaintext.
cryptanalysis :: ((Char, Char),(Char,Char)) -> ((Char,Char),(Char, Char)) -> Matrix.Matrix Int
cryptanalysis ((c1,c2),(p1,p2)) ((c3,c4),(p3,p4)) = Matrix.multStd (cipherM) (Util.inverseModMatrix 26 plainM)
    where plainM = Matrix.fromList 2 2 (map Util.toDigit [p1, p3, p2, p4])
          cipherM = Matrix.fromList 2 2 (map Util.toDigit [c1, c3, c2, c4])
