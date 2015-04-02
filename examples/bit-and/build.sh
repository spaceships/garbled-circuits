#!/bin/bash

cabal sandbox init
cabal sandbox add-source ../../
cabal install --only-dependencies
cabal install --bindir=.
