#!/usr/bin/env bash

if ! shellcheck \
     --exclude=SC2034 "$0"; then
    exit 1
fi

shopt -s extglob
set -o errexit
set -o nounset

msg="colecho -i --"
log="colecho --"
err="colecho -e --"

$msg starting

target_display=":1"

DISPLAY=":1"

if pgrep -x Xephyr; then
    $msg Xephyr instance is already running
else
    target_display=":$RANDOM"
    $msg Xephyr instance is missing, launching new one on display $target_display
    Xephyr -resizeable -sw-cursor -retro -ac $target_display
fi

DISPLAY=$target_display

target_file="gtk_application.nim"
output_file=$target_file.bin
tmp_output_file=$target_file.tmp.bin
build_status=0
function run_build () {
    $msg Running compilation of $target_file
    $log Outputting to $tmp_output_file

#    --hotCodeReloading:on       \


    nim c                           \
        --out:$tmp_output_file      \
        --hints:off                 \
        --verbosity:0               \
        --forceBuild \
        --nimCache:nimcache         \
        --warning[UnusedImport]:off \
        $target_file

    build_status=$?

    if [[ $build_status ]]; then
        $log Compilation completed succesfully "($build_status)"
        mv -v $tmp_output_file $output_file
    else
        $err Errors during compilation
    fi


}

function auto_rebuild () {
    $msg Started file change monitoring
    run_build
    after_rebuild
    inotifywait \
        -qe close_write,moved_to,create -m . |
    while read -r directory events filename; do
        if [[ $filename == !(.*|*.bin|*.tmp.*) ]]; then
            $log File "$filename" has changed
            run_build
            after_rebuild
        fi
    done
}

app_pid=""
rebuild_conf="restart-all"
function after_rebuild () {
    $msg Restarting whole application
    if [[ -z $app_pid ]]; then
        $msg App pid is empty, this is first run
    elif kill $app_pid; then
        $msg Killed application
    else
        $msg Application is not running
    fi

    $log Launching application "(./$output_file)" on display $DISPLAY
    ./$output_file &
    app_pid=$!
    $log Done

    execute_cnee_action "save-test"

    $log "Executing using xdotool"

    xdotool click 1
    xdotool click 1
}


# NOTE requires openbsd version of netcat

function execute_cnee_action () {
    $log Running cnee actions
    command=$1
    file="$command.xns"
    $msg Running actions from file "$file"
    if [[ ! -f $file ]]; then
        $err No file corresponds to action "$command"
    else
        cnee --replay --file "$file" 2> /dev/null
    fi
}

function change_configuration () {
    $log Updating configuration
    local command=$1
    #IFS="=" conf action < <(echo $command)
    conf=$(echo "$command" | cut -d'=' -f1)
    action=$(echo "$command" | cut -d'=' -f2)
    case $conf in
        "restart-all" )
            rebuild_conf=$conf
            ;;
        "set-cnee" )
            rebuild_conf="run-cnee"
            default_cnee="$action"
            ;;
    esac

}

command_port=12345
function listen_commands () {
    $msg Listening for commands on port $command_port
    nc -kl "$command_port" |
    while read -r kind command; do
        $log Accepted command "$command" of kind "$kind"
        echo "$command"
        case "$kind" in
            cnee )
                execute_cnee_action "$command"
                ;;
            config )
                change_configuration "$command"
                ;;
            * )
                $err Accepted undefined command kind: "$kind"
                ;;
        esac
    done
}

auto_rebuild &
listen_commands &
