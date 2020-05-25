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


#define socket_path "/tmp/coursework.socket"

using OFile   = std::ofstream;
using IFile   = std::ifstream;
using SStream = std::stringstream;

using Str = std::string;

template <class K, class V>
using Map = std::map<K, V>;

template <class T>
using Vec = std::vector<T>;

using UName = Str;

struct User {
    UName name;
    Str   password;
};

int  msgDepth = 0;
void msg(const char* text) {
    printf("%*s%s\n", msgDepth * 2, "", text);
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

void readConfig(Vec<User>& userList) {
    msg("Reading configuration file");
    IFile config;
    config.open("user-list.tmp");
    Str buf;
    while (std::getline(config, buf)) {
        User    usr;
        SStream line(buf);
        line >> usr.name >> usr.password;
    }
}


#define MAX_EVENTS 10

void loopConnection(int server_fd, const sockaddr_in& serv_addr) {
    Map<UName, Vec<Str>> messageQueue;
    msgInc();
    msg("Starting connection loop");

    epoll_event ev;
    epoll_event events[MAX_EVENTS];

    int epollfd = epoll_create1(0);
    if (epollfd == -1) {
        perror("epoll_create1");
        exit(EXIT_FAILURE);
    }

    ev.events  = EPOLLIN;
    ev.data.fd = server_fd;
    if (epoll_ctl(epollfd, EPOLL_CTL_ADD, server_fd, &ev) == -1) {
        perror("epoll_ctl: server_fd");
        exit(EXIT_FAILURE);
    }


    auto addrlen = sizeof(serv_addr);

    for (;;) {
        int nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_wait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            if (events[n].data.fd == server_fd) {
                int conn_fd = accept(
                    server_fd,
                    (struct sockaddr*)&serv_addr,
                    (socklen_t*)&addrlen);
                if (conn_fd == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }
                // setnonblocking(conn_fd);
                ev.events  = EPOLLIN | EPOLLET;
                ev.data.fd = conn_fd;
                if (epoll_ctl(epollfd, EPOLL_CTL_ADD, conn_fd, &ev)
                    == -1) {
                    perror("epoll_ctl: conn_fd");
                    exit(EXIT_FAILURE);
                }
            } else {
                // do_use_fd(events[n].data.fd);
            }
        }
    }

    msg("Stopped connection loop");
    msgDec();
}

void loopUsers(Vec<User>& userList) {
    sockaddr_un sock_addr = {AF_UNIX, socket_path};

    auto server_fd = socket(AF_INET, SOCK_STREAM, 0);

    sockaddr_in serv_addr;
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(51927);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    int opt = 1;
    msgInc();
    if (setsockopt(
            server_fd,
            SOL_SOCKET,
            SO_REUSEADDR | SO_REUSEPORT,
            &opt,
            sizeof(opt))
        != 0) {
        die("Cannot set sockopt");
    } else {
        msg("Set sockopt option");
    }

    if (bind(server_fd, (sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        die("Server bind failed");
    } else {
        msg("Server bind ok");
    }

    if (listen(server_fd, 5) != 0) {
        die("Server listen failed");
    } else {
        msg("Server listen start");
    }
    msgDec();

    loopConnection(server_fd, serv_addr);
}


void writeConfig(Vec<User>& userList) {
    msg("Writing configuration file");
    OFile config;
    config.open("user-list.tmp");
    for (const auto& usr : userList) {
        config << usr.name << usr.password;
    }
}

int main() {
    msg("Starting server application");

    Vec<User> userList;

    msgInc();

    readConfig(userList);
    loopUsers(userList);
    writeConfig(userList);
}
