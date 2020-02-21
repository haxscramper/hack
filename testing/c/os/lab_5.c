#include "common.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
    mlog("parent process");

    pid_t pid;

    milog("Initial process pid: ", getpid());

    if (pid = fork()) {
        mlog("PID value is non-zero, this is parent process");
        wait(NULL);
        mlog("Child process terminated");
    } else {
        mlog("PID value is zero, this is child process");
        pprefix();
        printf("Parent process id is %d\n", getpid());

        if (pid = fork()) {
            milog("Second fork parent pid:", getpid());
        } else {
            milog("Child pid: ", getpid());
            mlog("Child number three");
            mlog("Running ls");
            milog("Second fork group:", getpgrp());
            execl("/bin/ls", "");
            mlog("This code will not be executed");
        }
    }

    return 0;
}
