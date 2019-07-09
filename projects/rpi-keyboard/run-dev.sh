#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# bash
cd "$(dirname "$0")"
set -o nounset
set -o errexit

colecho -i1 'Running run-dev.sh'

cat << EOF | entr sh -c "clear ; colecho -i1 'Running entr ...' ; ./test.sh"
common.sh
setup_g_hid+wifi.sh
mount.sh
umount.sh
backup_default_config.sh
test.sh
EOF

