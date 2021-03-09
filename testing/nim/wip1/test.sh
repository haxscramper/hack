#!/usr/bin/env bash
# -*- coding: utf-8 -*- bash
set -o nounset
set -o errexit

function client_file_wait {
    serv_file="multiple_await.nim"
    clnt_file="await_client.nim"

    nim c -o:$serv_file.bin $serv_file
    nim c -o:$clnt_file.bin $clnt_file

    ./$serv_file.bin &

    ./$clnt_file.bin \
        --socket:12345 \
        --add-relative-file:temp.tmp

    # using temp.tmp without quotes results in runtime error with
    # connection being refused, and adding quotes creates runtime
    # error when printing port for connection
}

$1
