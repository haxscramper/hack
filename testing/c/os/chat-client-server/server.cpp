#include "common.hpp"
#include <optional>

struct User {
    UName name;
    Str   password;
    int   connection_fd;
};

using UsrList  = Vec<User>&;
using MsgQueue = Map<UName, Vec<Str>>;

User getUser(int connection, Vec<User>& userList) {
    for (const auto& usr : userList) {
        if (usr.connection_fd == connection) {
            return usr;
        }
    }

    throw std::logic_error("Cannot find user with given connection");
}

void processInput(UsrList userList, MsgQueue& queue, Str input) {
    SStream lines(input);
    Str     line;
    while (std::getline(lines, line, '\n')) {
        pind();
        printf(" -> %s\n", line.c_str());
    }
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

void loopConnection(
    int                server_fd,
    const sockaddr_in& serv_addr,
    Vec<User>&         userList) {
    MsgQueue messageQueue;
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

    msgInc();
    for (;;) {
        int nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
        if (nfds == -1) {
            perror("epoll_wait");
            exit(EXIT_FAILURE);
        }

        for (int n = 0; n < nfds; ++n) {
            if (events[n].data.fd == server_fd) {
                msg("Accepted connection");
                int client_fd = accept(
                    server_fd,
                    (struct sockaddr*)&serv_addr,
                    (socklen_t*)&addrlen);
                if (client_fd == -1) {
                    perror("accept");
                    exit(EXIT_FAILURE);
                }
                setnonblocking(client_fd);
                ev.events  = EPOLLIN | EPOLLET;
                ev.data.fd = client_fd;
                if (epoll_ctl(epollfd, EPOLL_CTL_ADD, client_fd, &ev)
                    == -1) {
                    perror("epoll_ctl: client_fd");
                    exit(EXIT_FAILURE);
                } else {
                    msg("Added new client to epoll");
                }
            } else {
                // msg("Accepted other event");
                // for(const auto& usr : )
                auto evt = events[n].events;
                if (evt & EPOLLIN) {
                    msg("Can read data from");
                    Str inBuf;
                    get_all_buf(events[n].data.fd, inBuf);
                    processInput(userList, messageQueue, inBuf);
                }
            }
        }
    }
    msgDec();

    msg("Stopped connection loop");
    msgDec();
}

void loopUsers(Vec<User>& userList) {
    auto        server_fd = socket(AF_INET, SOCK_STREAM, 0);
    sockaddr_in serv_addr;
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(SERVER_PORT);
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

    loopConnection(server_fd, serv_addr, userList);
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
