#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>

#include <netinet/in.h>
#include <sys/epoll.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/wait.h>


using OFile   = std::ofstream;
using IFile   = std::ifstream;
using SStream = std::stringstream;

using Str = std::string;

template <class K, class V>
using Map = std::map<K, V>;

template <class T>
using Vec = std::vector<T>;

using UName = Str;


#define SERVER_PORT 7800

#ifndef APP_TYPE
#    define APP_TYPE "\e[42msrv:\e[0m "
#endif

int msgDepth = 0;

void pind() {
    printf(APP_TYPE "%*s", msgDepth * 2, "");
}

void msg(const char* text) {
    pind();
    printf("%s\n", text);
}

void die(const char* text) {
    printf("!!! %*s%s\n", msgDepth * 2, "", text);
    printf("!!! %*sAborting program\n", msgDepth * 2, "");
    abort();
}

void msgInc() {
    msg("---");
    ++msgDepth;
};

void msgDec() {
    --msgDepth;
    msg("---");
}


void get_all_buf(int sock, std::string& inStr, int maxSize = 2048) {
    int  n = 1, total = 0, found = 0;
    char c;
    char temp[1024 * 1024];
    // Keep reading up to a '\n'
    int idx = 0;
    while (!found && (idx < maxSize)) {
        ++idx;
        n = recv(sock, &temp[total], sizeof(temp) - total - 1, 0);
        if (n == -1) {
            /* Error, check 'errno' for more details */
            break;
        }
        total += n;
        temp[total] = '\0';
        found       = (strchr(temp, '\n') != 0);
    }

    if (!(idx < maxSize)) {
        msg("Max message size reached");
    }
    inStr = temp;
}

// Setup nonblocking socket
int setnonblocking(int sockfd) {
    fcntl(sockfd, F_SETFL, fcntl(sockfd, F_GETFD, 0) | O_NONBLOCK);
    return 0;
}
#define MAX_EVENTS 10
