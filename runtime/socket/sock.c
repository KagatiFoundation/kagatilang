#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdint.h>

#include "typedefs.h"

struct sockaddr_in address;
socklen_t addrlen;

typedef void thisptr;

void __BICH_POSIX__close_socket(Bich_Int fd) {
    close(fd);
}

void __BICH_POSIX__write_socket(Bich_Int fd, const char* msg, Bich_Int size) {
    send(fd, msg, size, 0);
}

void __BICH_POSIX__listen_socket(Bich_Int fd, Bich_Int backlog) {
    if (listen(fd, backlog) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
}

Bich_Int __BICH_POSIX__accept_socket_conn(Bich_Int fd) {
    int new_socket = accept(fd, (struct sockaddr*)&address, &addrlen);
    if (new_socket < 0) {
        perror("accept");
        exit(EXIT_FAILURE);
    }
    return new_socket;
}

Bich_Int __BICH_POSIX__create_socket(Bich_Int port) {
    int opt = 1;
    addrlen = sizeof(address);

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }

    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt))) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr*)&address, sizeof(address)) < 0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }
    return server_fd;
}