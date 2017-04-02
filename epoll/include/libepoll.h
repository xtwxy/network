#ifndef libepoll_H
#define libepoll_H

#include <netdb.h>

typedef struct epoll_io_request_queue_node epoll_io_request_queue_node_s;
typedef struct epoll_io_request_queue epoll_io_request_queue_s;
typedef struct dispatch_context dispatch_context_s;

typedef void (*epoll_dispatch_cb)(void* dispatch_context_s);

struct epoll_stream_request {
	void* bytes;
	int length;
};

struct epoll_datagram_request {
	void* bytes;
	int length;
	struct sockaddr_in peer_addr;
};

struct epoll_io_request_queue_node {
	void * request;
	struct epoll_io_request_queue_node* next;
};

typedef void (*epoll_push_back_cb)(epoll_io_request_queue_s*, void*);
typedef void* (*epoll_pop_front_cb)(epoll_io_request_queue_s*);
typedef void* (*epoll_get_front_cb)(epoll_io_request_queue_s*);
typedef void* (*epoll_get_back_cb)(epoll_io_request_queue_s*);
typedef void (*epoll_empty_queue_cb)(dispatch_context_s* user_data);

struct epoll_io_request_queue {
	epoll_push_back_cb push_back;
	epoll_pop_front_cb pop_front;
	epoll_get_front_cb get_front;
	epoll_get_back_cb get_back;
	epoll_empty_queue_cb on_push_to_empty_queue;
	epoll_io_request_queue_node_s* front;
	epoll_io_request_queue_node_s* back;
	dispatch_context_s* context;
};

struct dispatch_context {
	int fd;
	void* user_data;
	epoll_io_request_queue_s* write_queue;
	epoll_dispatch_cb dispatch;
};


extern void epoll_print_hello ();
epoll_io_request_queue_s* epoll_create_write_request_queue();

#endif
