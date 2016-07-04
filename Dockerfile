FROM haskell
RUN cabal update
RUN cabal install http://ghcjs.luite.com/ghc-8.0.tar.gz
RUN ghcjs-boot
