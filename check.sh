#!/bin/sh

hlint ./src ./test ./bench ./examples && \
stack build --test --bench --haddock --ghc-options "-Werror" --no-run-benchmarks
