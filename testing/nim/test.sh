#!/usr/bin/env bash
# -*- coding: utf-8 -*- bash
set -o nounset

function build {
    file="$1"

    colecho -L "start $file"

    nim c                             \
        --cc:tcc                      \
        --verbosity:0                 \
        --hints:off                   \
        --debugger:native \
        -o:"$file.bin"                \
        -d:nimOldCaseObjects          \
        -d:npegTrace \
        --expandMacro:enter \
        -d:npegBackStackSize=16 \
        --warning[CaseTransition]:off \
        "$file"

        # -d:npegGraph \
    build_code="$?"

    colecho -L "end"
    echo


    if [[ "$build_code" != "0" ]]; then
        colecho -e "Build failed"
        return 1
    fi

}
function test_colecho {
    clear
    fd -e nim | xargs wc -l | sort -n | tail -n 1
    echo "Rebuilding ... "
    nim c \
        --cc:tcc \
        --verbosity:0 \
        --hints:off \
        -o:colecho_cli.nim.bin \
        colecho_cli.nim

}

function test_builder {
    clear
    killall fsm-build
    colecho --gtest -e "Starting test"
    fd -e nim | xargs wc -l | sort -n | tail -n 1
    colecho -b "Rebuilding ... "

    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    test_file="test.tmp.pl"
    rm -f "$test_file"
    echo -e '\n' | fsm-build dev "$test_file" &
    sleep 1
    echo "say 'test file mod';" >> "$test_file"

}

function test_create_script {
    build "create_script.nim"
    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    test_file="test.tmp.pl"
    echo -en "0\n" | create-script $test_file
    colecho "Hello"
    stat $test_file
    rm -f $test_file

    return 0

    killall fsm-build
    rm -f "$test_file"
    start-coding.sh "$test_file" &
    coding_pid="$!"

    sleep 1

    echo "say 'another test';" >> "$test_file"
    logm="log1 'Using log1'"
    echo "$logm;" >> "$test_file"
    echo "warn1 'Broken code'" >> "$test_file"


    wait "$coding_pid"
    colecho -g -e "Done testing"

    colecho "start-coding.sh returned"
}

function test_argparse {
    build "argparse2.nim"
    if [[ "$?" != 0 ]]; then
        return 0
    fi

    bin="argparse2.nim.bin"
    ./$bin
}

function test_fsm_build {
    build "fsm_build.nim"
    if [[ "$?" != 0 ]]; then
        colecho -e:2 "Build failed"
        return
    else
        colecho -i:2 "Done build"
    fi
    # colecho -g "ps grep"
    # ps -a | grep build
    # colecho -g "done"
    # cd ../wip
    # bin="fsm_build.nim.bin"

    # ln -f ../cli/fsm_build.nim.bin fsm-build
    cp -v fsm_build.nim.bin ../bin/fsm-build
    which fsm-build
    # echo -en "y\n0\n" | fsm-build dev test.tmp.java &
    echo -en "y\n0\n" | fsm-build dev test.tmp.dot &
    sleep 1
    rm -f test.tmp.dot
    sleep 2
    killall fsm-build
}

function test_flowchart_parser {
    bin="flowchart_generator.nim.bin"

    if [[ "$1" = *.txt ]]; then
        do_build="false"
    else
        do_build="true"
    fi

    full_run="true"

    if [[ "$do_build" = true ]]; then
        build "flowchart_generator.nim"

        if [[ "$build_code" != 0 ]]; then
            colecho -e:2 "Build failed"
            return
        else
            colecho -i:2 "Done build"
        fi
    fi

    testdir="test_files/flowchart_generator"
    while read -r test
    do
        ./$bin --test-line:"$test"
    done < <(cat $testdir/simple.txt)


    while read -r debug
    do
        ./$bin --debug-parse:"$debug"
    done < <(cat $testdir/debug.txt)


    if [[ "$full_run" != true ]]; then
        return
    fi


    rm -f $testdir/*.tmp.*
    find $testdir -name "*.txt.*" |
        xargs -i ./$bin --input:"{}" --output:"{}.tmp.dot"

    find $testdir -name "*.txt.*" |
        xargs -i ./$bin --input:"{}" --dump-tree --output:"{}.tmp.synt"

    while read -r dotfile
    do
        dot -Tpng $dotfile > $dotfile.png
    done < <(find -name "*.tmp.dot")
    ./generate_page.py
}

function test_extract_methods {
    bin="extract_methods.nim.bin"

    build "extract_methods.nim"
    if [[ "$build_code" != 0 ]]; then
        colecho -e:2 "Build failed"
        return
    else
        colecho -i:2 "Done build"
    fi

    ./$bin --input-file:"test_files/extract_method/test.java" \
           --output-dir:"test_files/extract_method/"
}

#test_builder
#test_create_script
#test_argparse
# test_fsm_build
# test_flowchart_parser none
test_extract_methods

inotifywait -r -e close_write,moved_to,create -m . |
    while read -r directory events f; do
        if [[ "$events" = *"CLOSE_WRITE"* ]]; then
          if [[ "$f" = *.nim ]] ||
                 [[ "$f" = *.pegs ]] ||
                 [[ "$f" = *.txt ]] ||
                 [[ "$f" = *.txt.c ]] ||
                 [[ "$f" = *.py ]] ||
                 [[ "$f" != "test1.sh" && "$f" == *".sh" ]]; then
            clear
            #test_builder
            #test_create_script
            #test_argparse
            # test_fsm_build
            # test_flowchart_parser $f
            test_extract_methods
          fi
        fi
    done
