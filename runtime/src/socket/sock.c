#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdint.h>

#include "../../include/typedefs.h"

/**
 * Close a socket. Just call this when you’re done with it.
 * Nothing fancy, just closes the file descriptor.
 */
void __KG_POSIX__close_socket(Kg_Int fd) {
    if (close(fd) < 0) {
        perror("close");
        exit(EXIT_FAILURE);
    }
}

/**
 * Send a message over a socket.
 * Just shove the bytes through the socket, that’s it.
 * 
 * fd   - the socket
 * msg  - pointer to the stuff you want to send
 * size - number of bytes to send
 */
void __KG_POSIX__write_socket(Kg_Int fd, const char* msg, Kg_Int size) {
    send(fd, msg, size, 0);
}

/**
 * Make a socket start listening for incoming connections.
 * Basically tells the OS: “Hey, I’m ready to accept people.”
 *
 * fd      - the socket you want to listen on
 * backlog - how many pending connections you can queue
 */
void __KG_POSIX__listen_socket(Kg_Int fd, Kg_Int backlog) {
    if (listen(fd, backlog) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
}

/**
 * Accept a new incoming connection.
 * Returns a new socket for talking to the client.
 *
 * fd - the listening socket
 */
Kg_Int __KG_POSIX__accept_socket_conn(Kg_Int fd) {
    struct sockaddr_in address;
    socklen_t addrlen = sizeof(address);

    int new_socket = accept(fd, (struct sockaddr*)&address, &addrlen);
    if (new_socket < 0) {
        perror("accept");
        exit(EXIT_FAILURE);
    }
    return new_socket;
}

/**
 * Create a TCP socket, bind it to a port, and get it ready to listen.
 * Returns the socket descriptor.
 *
 * port - the port number to bind to
 */
Kg_Int __KG_POSIX__create_socket(Kg_Int port) {
    struct sockaddr_in address;
    socklen_t addrlen = sizeof(address);
    int opt = 1;

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

    if (bind(server_fd, (struct sockaddr*)&address, addrlen) < 0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }
    return server_fd;
}
