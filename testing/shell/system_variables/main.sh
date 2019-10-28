#!/usr/bin/env bash

echo "started main"
export testvar="value"
./build.sh &
pid=$1
echo "forked child"
wait $pid
