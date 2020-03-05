#include "common.h"

#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void term_evasion(int signal) {
    mlog("!!! Child process ignores signal");
}

void test_forks() {
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
        exit(0);
    }

    waitpid(pid, 0, 0);
}

void test_pipes() {
    int fd[2];
    pipe(fd);

    pid_t pid = fork();
    if (pid) {
        // Close reading side of the process
        // Write some data for child process to read
        const char* str = "-------test-----";
        write(fd[1], str, (strlen(str) + 1));
        mlog("Wrote string");

        // Parent process
        waitpid(pid, 0, 0);
    } else {
        // Child process

        // Close writing side of the process
        char buff[64];
        read(fd[0], buff, sizeof(buff));
        printf("got string [%s]\n", buff);

        exit(0);
    }
}

int main(int argc, char* argv[]) {
    mlog("parent process");

    test_forks();
    test_pipes();

    milog("Intial process terminated", getpid());
    return 0;
}
