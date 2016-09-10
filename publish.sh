#!/bin/sh

if ./check.sh; then
	cabal clean
	cabal configure
	cabal build
	cabal sdist --output-directory ./dist/package
        tar -czf ./dist/package.tar.gz ./dist/package
	cabal upload ./dist/package.tar.gz
fi
