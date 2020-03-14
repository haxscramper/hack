/* --- server.c --- */
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
    puts("started server");
    char sendBuff[1025];
    // SOCKET
    int listenfd = socket(AF_INET, SOCK_STREAM, 0);

    struct sockaddr_in serv_addr;
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port        = htons(5800);

    int opt = 1;
    setsockopt(
        listenfd,
        SOL_SOCKET,
        SO_REUSEADDR | SO_REUSEPORT,
        &opt,
        sizeof(opt));

    // BIND
    bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
    // LISTEN
    listen(listenfd, 10);

    puts("Waiting for new connection");
    // ACCEPT
    int connfd = accept(listenfd, (struct sockaddr*)NULL, NULL);
    puts("Server accepted new connection");
    snprintf(sendBuff, sizeof(sendBuff), "eee1212239-9212");
    write(connfd, sendBuff, sizeof(sendBuff));


    close(connfd);
    close(listenfd);

    puts("Server finished");
}
