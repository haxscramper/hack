#include <iostream>

#include <fcntl.h>
#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>

#include <sys/socket.h>

#include <sys/un.h>
#include <unistd.h>

#include <sys/wait.h>

#include "errnames.hpp"


template <class T>
void mlog(T msg) {
    std::cout << "\033[32mLOG:\033[0m " << msg << std::endl;
}

template <class T>
void merr(T msg) {
    std::cout << "\033[31mERR:\033[0m " << msg << std::endl;
}

#define let const auto
#define var auto

#define socket_path "/tmp/lab7sock"
#define socket_lock "/tmp/lab7sock.lock"

const int forward_msg_size  = 256;
const int backward_msg_size = 32;


template <class T1, class T2>
void errtest(bool expression, T1 errmsg, T2 okmsg) {
    if (!expression) {
        merr(std::string("[✗] ") + errmsg);
        merr(get_err_symbol(errno) + " " + strerror(errno));
        exit(1);
    } else {
        mlog(std::string("[✓] ") + okmsg);
    }
}

void test_unix_domain_sockets() {
    let pid = fork();

    if (pid == 0) {
        // Child process, client
        int         sfd  = socket(AF_UNIX, SOCK_STREAM, 0);
        sockaddr_un addr = {AF_UNIX, socket_path};

#ifdef false
        int opt = 1;
        errtest(
            setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt))
                == 0,
            "Failed to set child socket options",
            "Succesfully set child socket options ok");
#endif

        errtest(
            bind(sfd, (sockaddr*)&addr, sizeof(addr)),
            "Child bind failed",
            "Child bind ok");

        errtest(
            connect(sfd, (sockaddr*)&addr, sizeof(addr)) == 0,
            "Child failed to connect to socket",
            "Child connected to socket");

        char outbuf[forward_msg_size];
        strcpy(outbuf, "input buffer start string");
        write(sfd, outbuf, sizeof(outbuf));

        exit(0);
    } else {
        // Parent process, server

        // Создать сокет
        int sfd = socket(AF_UNIX, SOCK_STREAM, 0);

        int opt = 1;

#ifdef false // REUSEADDR is only of INET sockets
        // https://www.linux.org.ru/forum/development/7573660
        // Настройка сокета
        errtest(
            setsockopt(
                sfd, //
                // Уровень настроек сокета - в данном случае мы хотим
                // указать параметры которые не зависят от протокола
                // (TCP/UDP).
                SOL_SOCKET,
                // Опции которые нужно установить для сокета
                // (принудительно подключится к сокету и адресу)
                SO_REUSEADDR | SO_REUSEPORT,
                &opt, // Опции которые нужно включить являются булевыми,
                      // так что для их активации передается едница
                sizeof(opt) // Длинна сегмента памяти с опицями
                )
                == 0,
            "Server socket configuration failed",
            "Server socket configuration ok");
#endif

        // Локальное соединение
        sockaddr_un address = {
            AF_UNIX, // Все процессы находятся на одном компьютере
            socket_path // Путь к файлу с сокетом
        };

        // Для повторного использования файла сокета расположенного на
        // том же пути требуется предварительно удалить файл.
        unlink(socket_path);

        // Подключаем сокет
        errtest(
            bind(
                sfd, // Файловый дескриптор

                // Указатель на адрес. Для того чтобы можно было
                // использовать как AF_UNIX так и AF_INET сокеты в одной
                // функции нужно сделать преобразование у указатель на
                // обобщеный сокет (sockaddr)
                (sockaddr*)&address,
                sizeof(address))
                == 0,
            "Server failed to bind socket",
            "Server socket bind ok");

        // Включаем прием соединений
        listen(sfd, 3);
        usleep(1000);

        let addrlen = sizeof(address);

        // Ожидаем подключения
        let connection = accept(
            sfd, // Дескриптор сокета
            (struct sockaddr*)&address, // Адрес клиента
            (socklen_t*)&addrlen        // Размер адреса
        );

        char buf[forward_msg_size];

        // Считать 256 байт в буфер. Блокирует до тех пор пока
        // требуемое количество байт не будет получено
        let valread = read(connection, buf, sizeof(buf));

        mlog("Recieved message from client");
        mlog(buf);

        char outbuf[32] = "hello world\n";
        send(connection, outbuf, sizeof(outbuf), 0);

        // errtest(shutdown(), T1 errmsg, T2 okmsg)
        close(sfd);
    }

    wait(NULL);
}

int main() {
    mlog("Hello");
    test_unix_domain_sockets();
}
