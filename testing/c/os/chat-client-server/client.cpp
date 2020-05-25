#define APP_TYPE "cln: "
#include "common.hpp"

int main() {
    auto        server_fd = socket(AF_INET, SOCK_STREAM, 0);
    sockaddr_in serv_addr;
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(SERVER_PORT);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

    msg("Starting client application");


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


    if (connect(server_fd, (sockaddr*)&serv_addr, sizeof(serv_addr))
        != 0) {
        die("Child failed to connect to socket ok");
    } else {
        msg("Child connected to socket");
    }

    Str buf;
    while (std::getline(std::cin, buf)) {
        buf      = buf + "\n";
        int size = buf.size();
        // send(server_fd, &size, sizeof(int), 0);
        send(server_fd, buf.c_str(), buf.size(), 0);
    }
}
