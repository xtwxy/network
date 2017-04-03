/*
 ============================================================================
 Name        : example-udp-echo.c
 Author      : Wangxy
 Version     :
 Copyright   : By Wangxy, all rights reserved.
 Description : Uses shared library to print greeting
 To run the resulting executable the LD_LIBRARY_PATH must be
 set to ${project_loc}/libepoll/.libs
 Alternatively, libtool creates a wrapper shell script in the
 build directory of this program which can be used to run it.
 Here the script will be called example-udp-echo.
 ============================================================================
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <arpa/inet.h>

#include "libepoll.h"

#define SOCK_DGRAM_STR "SOCK_DGRAM"
#define SOCK_STREAM_STR "SOCK_STREAM"
#define MAX_EVENTS 1024
#define BUFFER_SIZE 4096

void usage(char* progname) {
	printf("%s <address> [<port>/<service>] [SOCK_DGRAM/SOCK_STREAM]\n", progname);
}

void print_bytes(unsigned char * buf, size_t len) {
	for(size_t i = 0; i != len; ++i) {
		printf("%02x ", buf[i]);
	}
	printf("\n");
}

int epoll_init_dispatcher(char* node, char* service, void* user_data, int socket_type,
		epoll_dispatch_cb dispatch_cb) {
	int listen_sock, epollfd;

	listen_sock = create_socket_and_bind(node, service, socket_type); //SOCK_DGRAM

	if (make_fd_non_blocking(listen_sock) == -1) {
		perror("make_fd_non_blocking: listen_sock");
		exit(EXIT_FAILURE);
	}

	epollfd = epoll_create(10);
	if (epollfd == -1) {
		perror("epoll_create1");
		exit(EXIT_FAILURE);
	}

	dispatch_context_s* context = epoll_create_dispatch_context(epollfd,
			listen_sock, user_data, dispatch_cb);

	if (epoll_add_to_epoll_monitoring(epollfd, listen_sock, context) == -1) {
		perror("epoll_ctl: listen_sock");
		exit(EXIT_FAILURE);
	}

	return epollfd;
}

void my_epoll_dispach_cb(dispatch_context_s* context, struct epoll_event* ee) {
	ssize_t s = 0;
	unsigned char buf[BUFFER_SIZE];
	struct sockaddr src_addr;
	socklen_t src_addr_len = sizeof(src_addr);

	if((ee->events & EPOLLIN) == EPOLLIN) {

		do {
			s = recvfrom(context->fd,
					buf,
					sizeof(buf),
					0,
					&src_addr,
					&src_addr_len);

			if(s > 0) {
				if (src_addr.sa_family == AF_INET) {
					char s[INET6_ADDRSTRLEN];
					struct sockaddr_in* sa = (struct sockaddr_in*)&src_addr;
					inet_ntop(sa->sin_family, &(sa->sin_addr), s, sizeof s);
					printf("recvfrom() : %s:%d :\n", s, ntohs(sa->sin_port));
				}
				print_bytes(buf, s);

				sendto (context->fd,
						buf,
						s,
						0,
						&src_addr,
						src_addr_len);
			} else if(s < 0) {
				if(errno == EAGAIN) {
					// it's ok.
				} else {
					perror("recvfrom()");
				}
			} else if(s == 0) {

			}
		} while(s > 0);

	} else if((ee->events & EPOLLOUT) == EPOLLOUT) {
		printf("Write complete, nothing to write...\n");
	} else {
		printf("Unknown event: 0x%08x\n", ee->events);
	}
}

int main(int argc, char* argv[]) {
	void* user_data = NULL;

	if(argc != 3) {
		usage(argv[0]);
		return EXIT_FAILURE;
	}

	int epollfd = epoll_init_dispatcher(argv[1], // binding address
			argv[2], // binding port/service
			user_data,
			SOCK_DGRAM,
			my_epoll_dispach_cb);

	epoll_event_loop(epollfd, MAX_EVENTS);

	return EXIT_SUCCESS;
}
