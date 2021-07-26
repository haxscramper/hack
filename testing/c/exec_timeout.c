#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void exec_with_timeout(char* const* argv, int timeout, int kill_signal) {

    pid_t intermediate_pid = fork();
    if (intermediate_pid == 0) {
        pid_t worker_pid = fork();
        if (worker_pid == 0) {
            execv(argv[0], argv);
            _exit(0);
        }

        pid_t timeout_pid = fork();
        if (timeout_pid == 0) {
            sleep(timeout);
            _exit(0);
        }

        pid_t exited_pid = wait(NULL);
        if (exited_pid == worker_pid) {
            kill(timeout_pid, SIGKILL);
        } else {
            kill(worker_pid, kill_signal);
        }
        wait(NULL); // Collect the other process
        _exit(0);   // Or some more informative status
    }
    waitpid(intermediate_pid, 0, 0);
}

int main() {
    puts("start");
    char* argv[1];
    argv[0] = strdup("echo 1");
    puts("Running child process");
    exec_with_timeout(argv, 6, SIGKILL);
    puts("done");
}
