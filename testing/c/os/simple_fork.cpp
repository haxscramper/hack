#include <iostream>


#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main() {

    std::cout <<   "pid изначального процесса       " << getpid() << "\n";

    pid_t pid = fork();
    if (pid == 0) {
      std::cout << "pid == 0 в дочернем процессе    " << pid << "\n";
      std::cout << "pid дочернего процесса          " << getpid() << "\n";
      std::cout << "pid родителя дочернего процесса " << getppid() << "\n";
    } else if (pid > 0) {
      std::cout << "pid > 0 в родительском процессе " << pid << "\n";
      std::cout << "pid родительского процесса      " << getpid() << "\n";
    }

    return 0;
}
