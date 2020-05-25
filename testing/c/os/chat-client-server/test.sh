#!/usr/bin/env bash
set -o nounset
set -o errexit

clang++ -g server.cpp -o server
clang++ -g client.cpp -o client

echo -e "u1 p1\nu2 p2" > user-list.tmp

# gdb ./server -q -x init.gdb &
./server >> /tmp/hh &

sleep 1

cat << EOF |  ./client &
/login u1 p1
/connect u2
First message
Second message
More messages
EOF

sleep 2

cat << EOF |  ./client &
/login u2 p2
/connect u1
Send reply
Send another reply
/server-shutdown
EOF
