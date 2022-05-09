#!/bin/bash
ghcid --command='stack ghci --main-is=try-phi:exe:try-phi' \
    --reload=./app/ -W \
    --reload=./src/ -r --restart=./try-phi.cabal