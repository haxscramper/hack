#include "common.h"

#include <memory.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/shm.h>
#include <sys/signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

void test_shared_memory() {
    // Create new memory segment or get access to existing one
    int memory_id = shmget(
        2, // Segment id 2
        4, // Four bytes
        IPC_CREAT | SHM_R | SHM_W);
    if (memory_id == -1) {
        merr("Failed to create memory segment");
        mlerr();
        sleep(100);
        exit(1);
    }

    if (fork()) {
        // Parent process
        milog("Attached parent process to memory segment:", getpid());
        milog("Using memory segnment id:", memory_id);

        // Get pointer to shared memory segment
        char* str = (char*)shmat(memory_id, NULL, 0);

        if ((size_t)str == -1) {
            merr("Failed to connect to memory");
            merr(strerror(errno));
        } else {
            puts(str);
            // Set value of shared memory segment
            memcpy(str, "hell", 4);
        }
    } else {
        // Child process
        milog("Attached child process to memory segment:", getpid());
        milog("Using memory segnment id:", memory_id);
        char* str = (char*)shmat(memory_id, NULL, 0);
        puts(str);
        // Unless deleted, shared memory segment will persist between
        // program restarts.
        memcpy(str, "9999", 4);
        exit(0);
    }
}

int main(int argc, char* argv[]) {
    mlog("Starting lab  6 ");
    test_shared_memory();

    return 0;
}
