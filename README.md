
# HDynTs: link cut trees and euler tours for dynamic trees

The aim for this library is to explore dynamic trees algorythms using haskell and functional programming, possibly achieving the sublinear results of the pointer based ones.

The library comes with a toy console application *hdynts* to explore the tree forest. Feel free to use it as an example to interface to the library.

For compilation you need a recent Haskell installation with working achieved cabal.

~~~~
cabal install
cabal test
cabal haddock --hyperlink-source
~~~~

Then you should find the *hdynts* executable in ```~/.cabal/bin/hdynts``` and documentation in ~~~~ dist/doc/html ~~~~

This library is based on fingertrees as logaritmic time access structure.


