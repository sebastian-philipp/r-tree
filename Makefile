dependencies:
	cabal install --dependencies-only

sandbox:
	cabal sandbox init


strict-test-dependencies:
	cabal install -ftest-strict --enable-tests --dependencies-only

strict-test:
	cabal configure -ftest-strict --enable-tests && cabal test

cabal-repl-strict:
	cabal repl strict
