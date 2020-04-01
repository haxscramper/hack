#!/usr/bin/env bash

set -o nounset
set -x
set -o errexit

echo "hello"

tmpdir=$(mktemp -d "/tmp/XXXXXXXXXXXXXXXX")
mkdir -p "${tmpdir}/dev"

cat /proc/mounts |
    grep '^/dev/' |
    awk '{print $1}' |
    sed -E 's/(.)p./1p1/' |
    sort |
    uniq |
    tee -a /dev/tty > $tmpdir/devices

echo "found" $(wc -l "$tmpdir/devices") "drives"

cat $tmpdir/devices |
    while read dev; do
        file=${tmpdir}${dev}
        if ! sudo smartctl -x --json $dev > "$file"; then
            ./final.py "$file"
        else
            ./final.py "$file"
        fi
    done

# rm -rf $tmpdir
