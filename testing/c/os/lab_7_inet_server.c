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
    int  listenfd = socket(AF_INET, SOCK_STREAM, 0);

    struct sockaddr_in serv_addr;
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port        = htons(5800);

    bind(listenfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));
    listen(listenfd, 10);

    puts("Waiting for new connection");
    int connfd = accept(listenfd, (struct sockaddr*)NULL, NULL);
    snprintf(sendBuff, sizeof(sendBuff), "eee\r\n");
    write(connfd, sendBuff, strlen(sendBuff));

    puts("New connection");

    close(connfd);
    close(listenfd);
}
