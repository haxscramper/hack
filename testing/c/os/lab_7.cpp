#include <iostream>

#include <fcntl.h>
#include <stdlib.h>
#include <sys/signal.h>
#include <sys/types.h>

#include <sys/socket.h>

#include <netinet/in.h>
#include <sys/un.h>
#include <unistd.h>

#include <sys/wait.h>

#include "errnames.hpp"
#include <limits.h>


#define let const auto
#define var auto

/// Вывод логов
template <class T>
void mlog(T msg) {
    std::cout << "\033[32mLOG:\033[0m " << msg << std::endl;
}

/// Вывод отформатированных сообщениях об ошибке
template <class T>
void merr(T msg) {
    std::cout << "\033[31mERR:\033[0m " << msg << std::endl;
}

/// Размер буфера для первого сообщения
const int forward_msg_size = 256;
/// Размер буфера для второго сообщения
const int backward_msg_size = 32;


/// Файл для UNIX domain сокетов
#define socket_path "/tmp/lab7sock"
// #define socket_lock "/tmp/lab7sock.lock"

/// Если `expression` является истинным то вывесто сообщения о
/// соответствии услови, в противном случае вывести сообщения об
/// ошибки и п код последней ошибки (получается из `errno`).
/// Возвращаемое значение обратно аргументу - это позволяет делать
/// дополнительные действия при ошибках (например `if (errtest(...)) {
/// /* код для обработки в случае ошибок */ }`)
template <class T1, class T2>
bool errtest(bool expression, T1 errmsg, T2 okmsg) {
    if (!expression) {
        merr(std::string("[✗] ") + errmsg);
        merr(get_err_symbol(errno) + " " + strerror(errno));
        return true;
    } else {
        mlog(std::string("[✓] ") + okmsg);
        return false;
    }
}

/// Тест для inet domain сокетов с использованием отдельных программ
void test_inet_domain_sockets() {
    char _cwd[PATH_MAX];
    // Получить путь к текущей директории
    getcwd(_cwd, sizeof(_cwd));
    let cwd    = std::string(_cwd);
    let server = cwd + "/server"; // Путь в исполняемомо файлу сервера
    let client = cwd + "/client"; // Путь к исполняемому файлу клиента

    pid_t serv_pid = fork();
    if (serv_pid == 0) {
        // Запускаем сервер
        errtest(
            execl(server.c_str(), "") == 0, "Failed to run server", "");
    }

    pid_t clnt_pid = fork();
    if (clnt_pid == 0) {
        // Запускаем клиент
        errtest(
            execl(client.c_str(), "") == 0, "Failed to run client", "");
    }

    int rets;
    waitpid(serv_pid, &rets, 0); // Ждем пока сервер не завершит работу
    waitpid(clnt_pid, &rets, 0); // Ждем клиент

    puts("Finished inet socket test");
}

/// Тест для сокетов с использованием дочернего процесса
void test_inet_domain_sockets_2() {
    sockaddr_in serv_addr;
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(51927);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    let pid = fork();
    if (pid == 0) {
        ////////////////////  create socket  /////////////////////
        let sfd = socket(AF_INET, SOCK_STREAM, 0);

        int opt = 1;
        errtest(
            setsockopt(
                sfd,
                SOL_SOCKET,
                SO_REUSEADDR | SO_REUSEPORT,
                &opt,
                sizeof(opt))
                == 0,
            "Failed to set child socket options",
            "Succesfully set child socket options ok");

        // /////////////////////////  bind  /////////////////////////
        // if (errtest(
        //         bind(sfd, (sockaddr*)&serv_addr, sizeof(serv_addr)) ==
        //         0, "Child client failed to bind inet socket", "Child
        //         client inet socket bind ok")) {
        //     merr("Child closing socket");
        //     close(sfd);
        //     exit(1);
        // }

        ///////////////////////  connect  ////////////////////////
        errtest(
            connect(sfd, (sockaddr*)&serv_addr, sizeof(serv_addr)) == 0,
            "Child failed to connect to socket ok",
            "Child connected to socket");

        mlog("Client sending test message ...");
        char outbuf[forward_msg_size];
        strcpy(outbuf, "!!! MESSAGE TO PARENT OVER SOCKET !!!");
        send(sfd, outbuf, sizeof(outbuf), 0);
        mlog("Client done test");

        std::string out  = "!!! Test string with undefined length !!!";
        int         size = out.size();
        mlog("Sending message with size " + std::to_string(size));
        send(sfd, &size, sizeof(int), 0);
        send(sfd, out.c_str(), out.size(), 0);
        mlog("Client done sized");

        // close(sfd);
        exit(0);
    } else {
        ////////////////////  create socket  /////////////////////
        let sfd = socket(AF_INET, SOCK_STREAM, 0);

        int opt = 1;

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
                SO_REUSEADDR | SO_REUSEPORT, // нужно для того чтобы
                                             // сокет переиспользовал
                                             // уже сущетсвующи адрес
                &opt, // Опции которые нужно включить являются
                      // булевыми, так что для их активации передается
                      // едница
                sizeof(opt) // Длинна сегмента памяти с опицями
                )
                == 0,
            "Server socket configuration failed",
            "Server socket configuration ok");

        /////////////////////////  bind  /////////////////////////
        errtest(
            bind(sfd, (sockaddr*)&serv_addr, sizeof(serv_addr)) >= 0,
            "Parent server failed to bind inet socket",
            "Parent server socket bind ok");

        ////////////////////////  listen  ////////////////////////
        errtest(
            listen(sfd, 5) == 0,
            "Server failed to start listen",
            "Listen ok");

        ////////////////////////  accept  ////////////////////////
        let addrlen = sizeof(serv_addr);
        let conn_fd = accept(
            sfd, (struct sockaddr*)&serv_addr, (socklen_t*)&addrlen);

        mlog("Accepted connection");

        if (errtest(
                conn_fd != -1,
                "Failed to accept connection",
                "Connection accept ok")) {
            close(sfd);
            exit(1);
        }

        {
            char testbuf[forward_msg_size];
            let  valread = read(conn_fd, testbuf, sizeof(testbuf));
            mlog("Recieved message from client");
            mlog(testbuf);
        }

        int len;
        read(conn_fd, &len, sizeof(int));

        mlog("Recived message size: " + std::to_string(len));

        {
            char* buf = (char*)malloc(len);
            mlog("Created buffer");
            read(conn_fd, buf, len);
            mlog("Recieved message: [" + std::string(buf) + "]");

            free(buf);
        }

        close(conn_fd);
        // close(sfd);
        wait(NULL);
    }
}

void test_unix_domain_sockets() {
    // Локальное соединение
    sockaddr_un sock_addr = {
        AF_UNIX, // Все процессы находятся на одном компьютере
        socket_path // Путь к файлу с сокетом
    };


    let pid = fork();

    if (pid == 0) {
        // Child process, client
        int sfd = socket(AF_UNIX, SOCK_STREAM, 0);
        // sockaddr_un addr = {AF_UNIX, socket_path};


        errtest(
            bind(sfd, (sockaddr*)&sock_addr, sizeof(sock_addr)),
            "Child bind failed",
            "Child bind ok");

        errtest(
            connect(sfd, (sockaddr*)&sock_addr, sizeof(sock_addr)) == 0,
            "Child failed to connect to socket ok",
            "Child connected to socket");

        char outbuf[forward_msg_size];
        strcpy(outbuf, "!!! MESSAGE TO PARENT OVER SOCKET !!!");
        write(sfd, outbuf, sizeof(outbuf));

        char inbuf[backward_msg_size];
        recv(sfd, inbuf, backward_msg_size, 0);
        mlog("Recieved message from parent");
        mlog(inbuf);

        exit(0);
    } else {
        // Parent process, server

        // Создать сокет
        int sfd = socket(AF_UNIX, SOCK_STREAM, 0);


        // Для повторного использования файла сокета расположенного на
        // том же пути требуется предварительно удалить файл.
        unlink(socket_path);

        // Подключаем сокет

        // Для того чтобы можно было корректно использовать UNIX сокет
        // его нажно скастовать в обобщенный тип То есть bind
        // привязывает сокет описанный структурой типа sockaddr к
        // файловому дескрипотору. Это первые два аргумента. Ну и так
        // как размер структур для описания сетевых и юникс сокетов
        // отличается то требуется также передать туда и размер
        // структуры.

        errtest(
            bind(
                sfd, // Файловый дескриптор

                // Указатель на адрес. Для того чтобы можно было
                // использовать как AF_UNIX так и AF_INET сокеты в одной
                // функции нужно сделать преобразование у указатель на
                // обобщеный сокет (sockaddr)
                (sockaddr*)&sock_addr,
                sizeof(sock_addr))
                == 0,
            "Server failed to bind socket",
            "Server socket bind ok");

        // Включаем прием соединений
        listen(sfd, 3);
        // Задержка для +- последовательного выполнения действий
        // дочерним процессом и родительским.
        usleep(1000);

        let addrlen = sizeof(sock_addr);

        // Ожидаем подключения
        let connection = accept(
            sfd, // Дескриптор сокета
            (struct sockaddr*)&sock_addr, // Адрес клиента
            (socklen_t*)&addrlen          // Размер адреса
        );

        char buf[forward_msg_size];

        // Считать 256 байт в буфер. Блокирует до тех пор пока
        // требуемое количество байт не будет получено. Дескриптор
        // сокета используется в качестве обычного файла.
        let valread = read(connection, buf, sizeof(buf));

        mlog("Recieved message from client");
        mlog(buf);

        char outbuf[32] = "@@ hello world @@";
        // Используется функция `send` которая позволяет указывать
        // дополнительные опции для работы с сокетами - `write`/`read`
        // не позволяют этого делать.
        send(connection, outbuf, sizeof(outbuf), 0);

        // errtest(shutdown(), T1 errmsg, T2 okmsg)
        close(sfd);
    }

    wait(NULL);
    mlog("Finished UNIX domain socket test");
}

int main() {
    test_unix_domain_sockets();
    mlog("----");
    test_inet_domain_sockets();
    mlog("----");
    test_inet_domain_sockets_2();
    mlog("----");
    mlog("Completed all tests");
}
