#!/usr/bin/env bash


function display_progress () {
    next="-"


    # Fill terminal window only partially
    local fraction=2
    local maxw=$(perl -e "print('-' x ($(tput cols) / $fraction))")
    local empty=$(perl -e "print(' ' x ($(tput cols) / $fraction))")

    while read -r line; do
        echo -ne "\r$next"
        next="$next-"
        sleep 0.025
        if [[ $next == $maxw ]]; then
            next="-"
            echo -ne "\r$empty"
        fi
    done
}

for n in $(seq 0 100); do
    echo "test"
done | display_progress
