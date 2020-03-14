/* --- client.c --- */
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
    int                sockfd = 0, n = 0;
    char               recvBuff[1024];
    struct sockaddr_in serv_addr;

    sockfd                    = socket(AF_INET, SOCK_STREAM, 0);
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(5800);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    int opt = 1;
    setsockopt(
        sockfd,
        SOL_SOCKET,
        SO_REUSEADDR | SO_REUSEPORT,
        &opt,
        sizeof(opt));

    puts("Client put connection request");
    connect(sockfd, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

    puts("Client reading from connection");
    read(sockfd, recvBuff, sizeof(recvBuff) - 1);

    close(sockfd);
    printf("Client recived message: '%s'\n", recvBuff);

    puts("Client done");

    return 0;
}
