#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>

#include "libepoll.h"

void epoll_push_back(epoll_io_request_queue_s* q, void* r) {
	if (q == NULL || r == NULL)
		return;
	epoll_io_request_queue_node_s* n = (epoll_io_request_queue_node_s*) malloc(
			sizeof(epoll_io_request_queue_node_s));
	memset(n, 0, sizeof(epoll_io_request_queue_node_s));
	n->request = r;

	if (q->back != NULL) {
		q->back->next = n;
		q->back = n;
	} else {
		q->back = n;
		q->front = n;
		q->on_push_to_empty_queue(q->context);
	}
}

void* epoll_pop_front(epoll_io_request_queue_s* q) {
	if (q == NULL)
		return NULL;
	epoll_io_request_queue_node_s * front = q->front;
	if (front != NULL) {
		q->front = q->front->next;
	} else {
		q->back = NULL;
	}
	return front->request;
}

void* epoll_get_front(epoll_io_request_queue_s* q) {
	if (q == NULL || q->front == NULL)
		return NULL;
	return q->front->request;
}

void* epoll_get_back(epoll_io_request_queue_s* q) {
	if (q == NULL || q->back == NULL)
		return NULL;
	return q->back->request;
}

epoll_io_request_queue_s* epoll_create_io_request_queue(
		dispatch_context_s* context) {
	epoll_io_request_queue_s* q = (epoll_io_request_queue_s*) malloc(
			sizeof(epoll_io_request_queue_s));
	memset(q, 0, sizeof(epoll_io_request_queue_s));
	q->back = NULL;
	q->front = NULL;
	q->get_back = epoll_get_back;
	q->get_front = epoll_get_front;
	q->pop_front = epoll_pop_front;
	q->push_back = epoll_push_back;
	q->context = context;

	return q;
}

void epoll_dispose_io_request_queue(epoll_io_request_queue_s* q) {
	if (q == NULL)
		return;

	while (q->front != NULL) {
		void* t = q->front;
		free(t);
		q->front = q->front->next;
	}
	q->back = NULL;

	free(q);
}

dispatch_context_s* epoll_create_dispatch_context(int efd, int fd,
		void* user_data, epoll_dispatch_cb dispach_cb) {

	dispatch_context_s* d = (dispatch_context_s*) malloc(
			sizeof(dispatch_context_s));
	memset(d, 0, sizeof(dispatch_context_s));

	d->fd = fd;
	d->dispatch = dispach_cb;
	d->user_data = user_data;
	d->write_queue = epoll_create_io_request_queue(d);

	return d;
}

void epoll_dispose_dispatch_context(dispatch_context_s* d) {
	epoll_dispose_io_request_queue(d->write_queue);
	// FIXME: who's responsibility to free d->user_data?
	free(d->user_data);
	free(d);
}

int create_socket_and_bind(char* node, char *service, int socktype) {
	struct addrinfo hints;
	struct addrinfo *result, *rp;
	int s, sfd;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_INET; /* Return IPv4 and IPv6 choices */
	hints.ai_socktype = socktype; /* We want a TCP socket */
	hints.ai_flags = AI_PASSIVE; /* All interfaces */

	s = getaddrinfo(node, service, &hints, &result);
	if (s != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
		return -1;
	}

	for (rp = result; rp != NULL; rp = rp->ai_next) {
		sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
		if (sfd == -1)
			continue;

		s = bind(sfd, rp->ai_addr, rp->ai_addrlen);
		if (s == 0) {
			/* We managed to bind successfully! */
			break;
		}

		close(sfd);
	}

	if (rp == NULL) {
		fprintf(stderr, "Could not bind\n");
		return -1;
	}

	freeaddrinfo(result);

	return sfd;
}

int make_fd_non_blocking(int sfd) {
	int flags, s;

	flags = fcntl(sfd, F_GETFL, 0);
	if (flags == -1) {
		perror("fcntl");
		return -1;
	}

	flags |= O_NONBLOCK;
	s = fcntl(sfd, F_SETFL, flags);
	if (s == -1) {
		perror("fcntl");
		return -1;
	}

	return 0;
}

int epoll_add_to_epoll_monitoring(int efd, int fd, dispatch_context_s* context) {
	struct epoll_event event;

	event.events = EPOLLIN | EPOLLET; //EPOLLOUT
	event.data.ptr = context;
	if (epoll_ctl(efd, EPOLL_CTL_ADD, fd, &event) == 0) {
		// success
		return 0;
	} else {
		return -1;
	}
}

void epoll_event_loop(int epollfd, int max_events) {
	int nfds;
	dispatch_context_s* context;
	struct epoll_event* events = calloc(max_events, sizeof(struct epoll_event));

	for(;;) {
		nfds = epoll_wait(epollfd, events, max_events, -1);

		if(nfds == -1) {
			perror("epoll_wait()");
			break;
		}
		for (int i = 0; i < nfds; ++i) {
			context = events[i].data.ptr;
			context->dispatch(context, &events[i]);
		}
	}
	free(events);
}

int epoll_init(char* node,
		char* service,
		void* user_data,
		int socket_type,
		epoll_dispatch_cb dispatch_cb) {
	int listen_sock, epollfd;

	listen_sock = create_socket_and_bind(node, service, socket_type); //SOCK_DGRAM
	if(make_fd_non_blocking(listen_sock) == -1) {
		perror("make_fd_non_blocking: listen_sock");
				exit(EXIT_FAILURE);
	}
	epollfd = epoll_create1(0);
	if (epollfd == -1) {
		perror("epoll_create1");
		exit(EXIT_FAILURE);
	}

	dispatch_context_s* context = epoll_create_dispatch_context(epollfd,
			listen_sock,
			user_data,
			dispatch_cb);

	if (epoll_add_to_epoll_monitoring(epollfd, listen_sock, context) == -1) {
		perror("epoll_ctl: listen_sock");
		exit(EXIT_FAILURE);
	}
	return epollfd;
}
