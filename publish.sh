#!/bin/sh

if ./check.sh; then
	cabal clean
	cabal configure
	cabal build
	cabal sdist
	cabal upload ./dist/do-list-*.tar.gz
fi
