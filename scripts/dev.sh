#!/bin/bash

# stack run

inotifywait -r -m -e create -e moved_to -e modify ./|
    while read directory action file
        do
            if [[ "$file" =~ .*hs$ ]]; then
                stack run
            fi
        done