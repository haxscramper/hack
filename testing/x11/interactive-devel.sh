#!/usr/bin/env bash

export DISPLAY=:0
Xephyr -resizeable -dpi 40 -sw-cursor -retro -ac :1 &
export DISPLAY=:1

pid=0

function run() {
    echo "======================="

    target=$(cat target.txt)

    if ! make --quiet; then
        colecho -e:0 "Build failed"
    else
        colecho -i:0 "Running app"
        ./$target &
        pid=$!
        colecho -i:0 "Start done"
    fi

    echo "======================="
}

run

echo "monitoring $PWD ..."

inotifywait -m $PWD -e close_write |
    while read -r directory events filename; do
        echo "$events $filename"
        colecho -w:0 "Killing application"
        kill $pid
        run
    done

