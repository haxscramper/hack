#!/usr/bin/env bash

set -o nounset
set -o errexit

tmpdir=$(mktemp -d "/tmp/XXXXXXXXXXXXXXXX")
mkdir -p "${tmpdir}/dev"

echo "System drives: "
cat /proc/mounts |
    grep '^/dev/' |              # Отфильтровать те файлы которые не
                                 # начинаются с `/dev/`
    awk '{print $1}' |           # Использовать только первую колонку
    sed -E 's/(.)p./1p1/' |
    sort |
    uniq |
    tee -a /dev/tty > $tmpdir/devices

cat $tmpdir/devices |
    while read dev; do
        file=${tmpdir}${dev}
        echo "---"
        if ! sudo smartctl -x --json $dev > "$file"; then
            ./final.py "$file"
        else
            ./final.py "$file"
        fi
    done

# rm -rf $tmpdir
