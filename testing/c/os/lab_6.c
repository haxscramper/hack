#include "common.h"

#include <fcntl.h>
#include <memory.h>
#include <stdlib.h>
#include <sys/ipc.h>
#include <sys/msg.h>
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
        2, // Segment key 2
        4, // Four bytes
        IPC_CREAT | SHM_R | SHM_W);
    if (memory_id == -1) {
        merr("Failed to create memory segment");
        mlerr();
        sleep(100);
        exit(1);
    }

    pid_t pid;
    if (pid = fork()) {
        // Parent process
        milog("Attached parent process to memory segment:", getpid());
        milog("Parent Using memory segnment id:", memory_id);

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
        waitpid(pid, 0, 0);
    } else {
        // Child process
        milog("Attached child process to memory segment:", getpid());
        milog("Child using memory segnment id:", memory_id);
        char* str = (char*)shmat(memory_id, NULL, 0);
        puts(str);
        // Unless deleted, shared memory segment will persist between
        // program restarts.
        memcpy(str, "9999", 4);
        exit(0);
    }
}

typedef struct {
    long msg_type;
    char msg_buff[6];
} message_t;

void test_message_queue() {

    // Size of message queue struct minus single field for message
    // type
    const size_t msg_size = sizeof(message_t) - sizeof(long);

    // Создаем или получаем новую очередб
    int   msg_queue_id = msgget(IPC_PRIVATE, IPC_CREAT | 0600);
    pid_t pid          = fork();
    if (pid > 0) {
        // Parent process
        message_t output;
        output.msg_type = 1;
        // Устанавливаем текст в сообщении
        strcpy(output.msg_buff, "123456");
        // Добавляем сообщение в очередь
        if (msgsnd(msg_queue_id, &output, msg_size, 0) == -1) {
            merr("Failed to add message to queue");
            mlerr();
            exit(1);
        } else {
            mlog("Wrote message to queue");
        }
    } else {
        // Child process
        message_t input;
        int       rcv_stat = 0;
        // Получаем сообщение из очереди
        msgrcv(msg_queue_id, &input, msg_size, 1, 0);
        if (rcv_stat < 0) {
            merr("Failed to read message from queue");
            mlerr();
        } else {
            // Сообщение из очереди получено
            printf("Recieved message from queue [%s]\n", input.msg_buff);
            msgctl(msg_queue_id, IPC_RMID, NULL);
        }
        exit(0);
    }
}

#define errtest(value, errmsg, okmsg)                                     \
    if (value == -1) {                                                    \
        merr(errmsg);                                                     \
        mlerr();                                                          \
        exit(1);                                                          \
    } else {                                                              \
        mlog(okmsg);                                                      \
    }

union semun {
    int              val;
    struct semid_ds* buf;
    unsigned short*  array;
    struct seminfo*  _buf;
};

// Тест семафоров
void test_semaphores() {
    const size_t memsize = 1024 * 1024;
    // Создаем или получаем участок с раздеоенной памятью. IPC_PRIVATE
    // означает то что новый сегмент памяту должен быть выделен. (Из
    // `man "shmget(2)"`: The name choice IPC_PRIVATE was perhaps
    // unfortunate, IPC_NEW would more clearly show its function.)
    int mem_id = shmget(IPC_PRIVATE, memsize, IPC_CREAT | SHM_R | SHM_W);

    // Создаем новый набор семафоров, состоящий из 1 (одного) семафора
    int sem_id = semget(IPC_PRIVATE, 1, IPC_CREAT | 0666);

    // Проверка на ошибки
    errtest(sem_id, "Failed to create semaphore", "Created semaphores");

    // СОздаем клон процесса, id для набора самафоров и участка памяти
    // наследуется для дочернего процесса
    pid_t pid = fork();
    errtest(pid, "Fork failed", "Fork suceded");
    if (pid) {
        // Parent process
        // Подключаем разделенную память родительскому процессу
        char* buff = (char*)shmat(mem_id, NULL, 0);
        errtest(
            (long)buff,
            "Parent failed to read memory",
            "Parent memory open ok");

        // Создаем объединение которое будет использовано для установки
        // значений и ставим передаваемое значние
        union semun setval_arg;
        setval_arg.val = 7;

        // Устанавливаем значение первого (нулевого по индексу)
        // семафора из набора семафоров `sem_id`. Значение будет
        // установлено так как был использован аргумент `SETVAL`
        semctl(sem_id, 0, SETVAL, setval_arg);

        // Заполняем разделенную память случайными значениями и
        // производим задержки на случайный период времений
        for (int i = 0; i < memsize; ++i) {
            buff[i] = (char)(random() % 32);
            if (rand() % 128 > 125) {
                usleep(1);
            }
        }
        mlog("Filled memory");


        // Меняем значение семафора для того чтобы обозначить что из
        // разделенной памяти можно свободно читать.
        semctl(sem_id, 0, SETVAL, 0);

        // Ждем дочерний процесс
        waitpid(pid, 0, 0);

        // Удаляем участок с разделенной памятью так как он больше не
        // нужем (? в противном случае он будет существовать и *после*
        // завершения программы ?)
        shmdt(buff);
    } else {
        // Child process
        int semval = semctl(sem_id, 0, GETVAL, semval);
        mlog("Waiting for semaphore to indicate finish");

        // Проверяем что семафор не нулевой
        while (semval != 0) {
            semval = semctl(sem_id, 0, GETVAL, NULL);
            usleep(10);
        }

        mlog("Semaphore value is 0, can read memory");

        char* buff = (char*)shmat(mem_id, NULL, 0);
        errtest(
            (long)buff,
            "Child failed to read memory",
            "Child memory open ok");

        exit(0);
    }
}

int main(int argc, char* argv[]) {
    mlog("Starting lab  6 ");
    mlog("=========== shared memory ===============");
    test_shared_memory();

    mlog("=========== message queue ===============");
    test_message_queue();

    mlog("=========== semaphores ===============");
    test_semaphores();
    return 0;
}
