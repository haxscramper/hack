#!/usr/bin/env bash

RESPONSE="HTTP/1.1 200 OK\r\nConnection: keep-alive\r\n\r\n${2:-"OK"}\r\n"
# nc -l "{}"
# while { echo -en "$RESPONSE"; } | nc -l "${1:-8080}"; do
#     echo "================================================"
# done

nc -l "${1:-8080}"
