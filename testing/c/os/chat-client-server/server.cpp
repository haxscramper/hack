#include "common.hpp"
#include <optional>

struct User {
    UName name;
    Str   password;
    int   connection_fd;
    UName target;
};

using UsrList  = Vec<User>&;
using MsgQueue = Map<UName, Vec<Str>>;

template <typename Out>
void split(const std::string& s, char delim, Out result) {
    std::stringstream ss(s);
    std::string       item;
    while (std::getline(ss, item, delim)) {
        *(result++) = item;
    }
}

std::vector<std::string> split(const std::string& s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, std::back_inserter(elems));
    return elems;
}


User getUser(int connection, Vec<User>& userList) {
    for (const auto& usr : userList) {
        if (usr.connection_fd == connection) {
            return usr;
        }
    }

    throw std::logic_error("Cannot find user with given connection");
}


User getUser(Str name, Vec<User>& userList) {
    for (const auto& usr : userList) {
        if (usr.name == name) {
            return usr;
        }
    }

    throw std::logic_error("Cannot find user with given name");
}

void sendErr(int connection_fd, Str message) {
    message = "\e[41mERR:\e[0m " + message;
    send(connection_fd, message.c_str(), message.size(), 0);
}


void sendMessage(int connection_fd, Str message) {
    message = "\e[45mMSG:\e[0m " + message;
    send(connection_fd, message.c_str(), message.size(), 0);
}

User& getCredentials(Str login, Str password, UsrList list) {
    for (auto& usr : list) {
        if (usr.name == login && usr.password == usr.password) {
            return usr;
        }
    }

    throw std::logic_error("Login or password invalid ");
}

void sendMessage(const User& user, Str message) {
    sendMessage(user.connection_fd, message);
}


#define ALLUSERS for (User & usr : userList)

void processInput(
    UsrList   userList,
    MsgQueue& queue,
    Str       input,
    int       connection) {
    SStream lines(input);
    Str     line;
    while (std::getline(lines, line, '\n')) {
        Vec<Str> tokens = split(line, ' ');
        Str      cmd    = tokens[0];
        if (cmd == "/register") {
            Str name = tokens[1];
            Str pwd  = tokens[2];
            // pind();
            printf("Reg new user '%s' '%s'\n", name.c_str(), pwd.c_str());

            User usr;
            usr.name     = name;
            usr.password = pwd;

            userList.push_back(usr);
        } else if (cmd == "/login") {
            try {
                User& usr = getCredentials(tokens[1], tokens[2], userList);
                usr.connection_fd = connection;

                printf(
                    "Logged user %s #%d\n", tokens[1].c_str(), connection);
                if (queue.find(usr.name) != queue.end()
                    && queue[usr.name].size() > 0) {
                    sendMessage(
                        usr,
                        "There are "
                            + std::to_string(queue[usr.name].size())
                            + " new messages for you");

                    for (auto& msg : queue[usr.name]) {
                        sendMessage(usr, msg);
                    }
                }
            } catch (std::logic_error e) { sendErr(connection, e.what()); }
        } else if (cmd == "/server-shutdown") {
            ALLUSERS {
                sendMessage(usr, "Server is about to shut down");
            }
            msg("Server shutdown");
            exit(0);
        } else if (cmd == "/connect") {
            try {
                getUser(connection, userList).target = tokens[1];
            } catch (std::logic_error err) { msg(err.what()); }
        } else {
            try {
                const User& current = getUser(connection, userList);
                line                = current.name + ": " + line;
                try {
                    send(
                        getUser(current.target, userList).connection_fd,
                        line.c_str(),
                        line.size(),
                        0);
                } catch (std::logic_error target_err) {
                    msg("Stored message in queue");
                    queue[current.target].push_back(line);
                }
            } catch (std::logic_error current_err) {
                sendErr(
                    connection,
                    "Cannot send your message from #"
                        + std::to_string(connection) + " - need to login");
            }
        }
    }
}


void readConfig(Vec<User>& userList) {
    // msg("Reading configuration file");
    IFile config;
    config.open("user-list.tmp");
    Str buf;
    while (std::getline(config, buf)) {
        User    usr;
        SStream line(buf);
        line >> usr.name >> usr.password;
    }
}


void loopConnection(
    int                server_fd,
    const sockaddr_in& serv_addr,
    Vec<User>&         userList) {
    MsgQueue messageQueue;
    // msgInc();
    // msg("Starting connection loop");

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
            int fd = events[n].data.fd;
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
                auto evt = events[n].events;
                if (evt & EPOLLIN) {
                    // msg("Can read data from");
                    char buf[2048];
                    int  rc = read(fd, buf, 2048);
                    // get_all_buf(events[n].data.fd, inBuf);
                    Str inBuf(buf);
                    pind();
                    // printf("read %d bytes from remote\n", rc);
                    processInput(
                        userList, messageQueue, inBuf, events[n].data.fd);
                }
            }
        }
    }
    // msgDec();

    // msg("Stopped connection loop");
    // msgDec();
}

void loopUsers(Vec<User>& userList) {
    auto        server_fd = socket(AF_INET, SOCK_STREAM, 0);
    sockaddr_in serv_addr;
    memset(&serv_addr, '0', sizeof(serv_addr));

    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_port        = htons(SERVER_PORT);
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);

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

    if (bind(server_fd, (sockaddr*)&serv_addr, sizeof(serv_addr)) < 0) {
        die("Server bind failed");
    } else {
        // msg("Server bind ok");
    }

    if (listen(server_fd, 5) != 0) {
        die("Server listen failed");
    } else {
        // msg("Server listen start");
    }
    // msgDec();

    loopConnection(server_fd, serv_addr, userList);
}


void writeConfig(Vec<User>& userList) {
    // msg("Writing configuration file");
    OFile config;
    config.open("user-list.tmp");
    for (const auto& usr : userList) {
        config << usr.name << usr.password;
    }
}

int main() {
    // msg("Starting server application");

    Vec<User> userList;

    // msgInc();

    readConfig(userList);
    loopUsers(userList);
    writeConfig(userList);
}
