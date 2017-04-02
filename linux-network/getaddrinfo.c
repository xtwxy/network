#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>

void usage(char* progname) {
	fprintf(stderr, "Usage: %s <node> <service>\n", progname);
}

void *get_in_addr(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET)
        return &(((struct sockaddr_in*)sa)->sin_addr);
    return &(((struct sockaddr_in6*)sa)->sin6_addr);
}
in_port_t get_in_port(struct sockaddr *sa)
{
    if (sa->sa_family == AF_INET)
        return ntohs(((struct sockaddr_in*)sa)->sin_port);
    return ntohs(((struct sockaddr_in6*)sa)->sin6_port);
}

int main(int argc, char* argv[]) {
	struct addrinfo hints;
	struct addrinfo *result, *rp;

	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_family = AF_UNSPEC;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_PASSIVE;

	char* node = NULL;
	char* service = NULL;

	if(argc < 2) {
		usage(argv[0]);
		return EXIT_FAILURE;
	} else if(argc == 2) {
		service = argv[1];
	} else if(argc == 3) {
		node = argv[1];
		service = argv[2];
	} else {
		usage(argv[0]);
		return EXIT_FAILURE;
	}
	int s = getaddrinfo(node, service, &hints, &result);
       	if(s != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(s));
		return EXIT_FAILURE;
	}
	
	for(rp = result; rp != NULL; rp = rp->ai_next)
	{
		fprintf(stdout, "ai_flags: %d\n", rp->ai_flags);
		fprintf(stdout, "ai_family: %d\n", rp->ai_family);
		fprintf(stdout, "ai_socktype: %d\n", rp->ai_socktype);
		fprintf(stdout, "ai_protocol: %d\n", rp->ai_protocol);
		fprintf(stdout, "ai_addrlen: %d\n", rp->ai_addrlen);
		fprintf(stdout, "ai_canonname: %s\n", rp->ai_canonname);
		fprintf(stdout, "rp->ai_addr->sa_family: %d\n", rp->ai_addr->sa_family);
		char s[INET6_ADDRSTRLEN];
		inet_ntop(rp->ai_family, get_in_addr(rp->ai_addr), s, sizeof s);
		fprintf(stdout, "%s\n", s);
		fprintf(stdout, "%d\n", get_in_port(rp->ai_addr));
	}

	return EXIT_SUCCESS;
}
