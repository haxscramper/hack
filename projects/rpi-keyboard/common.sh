#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash

run_mode="nottest"
#run_mode="test"

space() {
    colecho -l2 " "
}

run() {
    colecho -w0 -- "$@"
    if [ $run_mode != "test" ]; then
        sudo "$@"
    fi
}

show () {
    colecho -i0 "Showing $1"
    if [ $run_mode != "test" ]; then
        space
        cat "$1" | sed 's/^/    /'
        space
    fi
}

write() {
    colecho -w2 -- "Writing to $2:"
    echo -e "$1" | sed 's/^/    /'
    if [ $run_mode != "test" ]; then
        space
        echo -e "$1" | sudo tee -a "$2" > /dev/null
        space
    fi
}

print_partitions() {
    colecho -i2 "Block devices:"

    { lsblk | head -n1 ; lsblk | grep "sd$1" ; } | sed 's/^/    /'
}

get_partitions () {
    local I_DEVICE="/dev/sd$1"

    local parts=$(lsblk -b |
                      grep -e "sd${1}"[0-9] |
                      sed -E 's/^.{5}//' |
                      awk '{ print $4" "$1}' |
                      sort -n |
                      cut -d' ' -f2)

    case "$2" in
        boot)
            echo "${I_DEVICE}$(echo -e "$parts" | head -n1)"
            ;;
        root)
            echo "${I_DEVICE}$(echo -e "$parts" | tail -n1)"
            ;;
    esac

}
