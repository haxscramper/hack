#include "common.h"

#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void term_evasion(int signal) {
    merr("Child process ignores signal");
}

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
            if (pid = fork()) {
                milog("Forked third time, child pid is", pid);
                sleep(1);
                kill(pid, SIGTERM);
            } else {
                mlog("Child process, do sleep(1000)");
                signal(SIGTERM, &term_evasion);
                sleep(1000);
            }
        } else {
            milog("Child pid: ", getpid());
            mlog("Child number three");
            mlog("Running ls");
            milog("Second fork group:", getpgrp());
            execl("/bin/ls", "");
            mlog("This code will not be executed");
        }
    }

    milog("Intial process terminated", getpid());
    return 0;
}
