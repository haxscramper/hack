#!/usr/bin/env bash
set -o nounset
set -o errexit

clang++ server.cpp -o server
clang++ client.cpp -o client

./server &

cat << EOF | ./client &
/register user-1 password-1
/login user-1 password-1
/connect user-2
First message
Second message
EOF

cat << EOF | ./client &
/register user-1 password-1
/login user-2 password-2
/connect user-1
Send reply
Send another reply
/clear-user-list
/server-shutdown
EOF
