#!/bin/bash
ghcid --command='stack ghci' \
    --reload=./app/ -W \
    --reload=./src/ -r --restart=./try-phi.cabal