#define APP_TYPE "\e[43mcln:\e[0m "
#include "common.hpp"

int main() {
    // auto        server_fd = socket(AF_INET, SOCK_STREAM, 0);
    // sockaddr_in serv_addr;
    // memset(&serv_addr, '0', sizeof(serv_addr));

    // serv_addr.sin_family      = AF_INET;
    // serv_addr.sin_port        = htons(SERVER_PORT);
    // serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    msg("Starting client application");

    sockaddr_un serv_addr = {AF_UNIX, socket_path};
    auto        server_fd = socket(AF_UNIX, SOCK_STREAM, 0);


    int opt = 1;
    // msgInc();
    if (setsockopt(
            server_fd,
            SOL_SOCKET,
            SO_REUSEADDR | SO_REUSEPORT,
            &opt,
            sizeof(opt))
        != 0) {
        die("Cannot set sockopt");
    } else {
        // msg("Set sockopt option");
    }


    if (connect(server_fd, (sockaddr*)&serv_addr, sizeof(serv_addr))
        != 0) {
        die("Child failed to connect to socket ok");
    } else {
        // msg("Child connected to socket");
    }


    {
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


        ev.events  = EPOLLIN;
        ev.data.fd = STDIN_FILENO;
        if (epoll_ctl(epollfd, EPOLL_CTL_ADD, STDIN_FILENO, &ev) == -1) {
            perror("epoll_ctl: stdin");
            exit(EXIT_FAILURE);
        }

        while (1) {
            int nfds = epoll_wait(epollfd, events, MAX_EVENTS, -1);
            if (nfds == -1) {
                perror("epoll_wait");
                exit(EXIT_FAILURE);
            }

            for (int n = 0; n < nfds; ++n) {
                auto evt = events[n].events;
                auto fd  = events[n].data.fd;
                if (fd == STDIN_FILENO) {
                    if (evt & EPOLLIN) {
                        Str buf;
                        if (std::getline(std::cin, buf)) {
                            buf      = buf + "\n";
                            int size = buf.size();
                            send(server_fd, buf.c_str(), buf.size(), 0);
                        }
                    }
                } else if (fd == server_fd) {
                    char buf[2048];
                    if (evt & EPOLLIN) {
                        int rc = recv(fd, buf, 2048, 0);
                        if (rc > 0) {
                            std::cout << std::string(buf, rc);
                        } // else {
                        //     std::cout << "recieved " << rc
                        //               << " from server" << std::endl;
                        // }
                    }
                } else {
                    printf("Can read from FD: %d", fd);
                }
            }
        }
    }
}
