# classic-crypto
Super basic classic crypto algorithms such as Affine, Hill Digraph, Shift, and Vigenere Square implemented in pure Haskell. classic-crypto was created for a class taken on introductory cryptology. Its purpose was to expedite boring and otherwise trival-yet-time-consuming computations rather than perform them by hand. Originally it was used interactively to encode and decode simple messages.

It is doubtful I will need this library again as I have not used Haskell since this was completed. It was a learning aid and I do not intend to expand upon it at this time. That said, feel free to submit pull requests or issues and I will try to assist.

## Use

This library was created to be used interactively via a REPL. Chances are it could be used programatically, but I never tried as doing so was not necessary for my needs. Let me know if it works.

For the most part the modules are broken down by the algorithm they encipher and decipher. Core function naming should be consistent (more or less) across modules. Util.hs plays are role in centralizing often used functions such as I/O.

Functions named along the lines of `cryptanalysis` typically mean the function is used for attempting to break the cipher.
  
Development was done on the Haskell Platform.
  
Only strings of the normal case insensitive English alphabet (A-Z) were used.
