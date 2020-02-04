#!/usr/bin/env bash
shopt -s extglob

msg="colecho -i --"
log="colecho --"
err="colecho -e --"

$msg starting

target_display=":1"

DISPLAY=":1"

if $(ps | grep -q Xephyr); then
    $msg Xephyr instance is already running
else
    target_display=":$RANDOM"
    $msg Xephyr instance is missing, launching new one on display $target_display
    Xephyr -resizeable -sw-cursor -retro -ac $target_display
fi

DISPLAY=$target_display

target_file="gtk_application.nim"
output_file=$target_file.bin
build_status=0
function run_build () {
    $msg Running compilation of $target_file

    nim c                  \
        --cc:tcc           \
        --out:$output_file \
        --hints:off        \
        --verbosity:0      \
        $target_file

    build_status=$?

    if [[ $build_status ]]; then
        $log Compilation completed succesfully
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
            $log File $filename has changed
            run_build
            after_rebuild
        fi
    done
}

app_pid=""
rebuild_conf="restart-all"
default_cnee="none"
function after_rebuild () {
    $log Executing after rebuild action
    case $rebuild_conf in
        "restart-all" )
            $msg Restarting whole application
            if [[ $(ps | grep -q "$app_pid") && -z $app_pid ]]; then
                $msg Application is still running, executing kill
                kill $app_pid
            elif [[ -z $app_pid ]]; then
                $msg App pid is empty, this is first run
            fi

            $log Launching application on display $DISPLAY
            ./$output_file &
            app_pid=$!
            $log Done

            ;;
        "run-cnee" )
            $msg Running cnee action
            execute_cnee_action $default_cnee
            ;;
        * )
            $err Unknown after rebuild action $rebuild_conf
            ;;
    esac

}


# NOTE requires openbsd version of netcat

function execute_cnee_action () {
    $log Running cnee actions
    command=$1
    case $default_cnee in
        "none" )
            $msg Default action is none
            ;;
        * )
            file=$command.xns
            $msg Running actions from file $file
            if [[ ! -f $file ]]; then
                $err No file corresponds to action $command
            fi
            ;;
    esac

}

function change_configuration () {
    $log Updating configuration
    local command=$1
    IFS="=" conf action < <($echo $command)
    case $conf in
        "restart-all" )
            rebuild_conf=$conf
            ;;
        "set-cnee" )
            $rebuild_conf="run-cnee"
            default_cnee=$action
            ;;
    esac

}

command_port=12345
function listen_commands () {
    $msg Listening for commands on port $command_port
    nc -kl $command_port |
    while read -r kind command; do
        $log Accepted command $command of kind $kind
        echo $command
        case $kind in
            cnee )
                execute_cnee_action $command
                ;;
            config )
                change_configuration $command
                ;;
            * )
                $err Accepted undefined command kind: $kind
                ;;
        esac
    done
}

auto_rebuild &
listen_commands &
