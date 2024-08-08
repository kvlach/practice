#include <sys/socket.h>
#include <unistd.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#define SOCK_PATH "/home/slow/src/my/practice/ip_socket"

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

int main() {
	int err;

	int ip_sock = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	if (ip_sock < 0) {
		logerr();
		return 1;
	}

	const struct sockaddr *addr = { .sa_family = AF_INET, .sa_data = "127.0.0.1" };
	err = bind(ip_sock, addr, sizeof(addr));
	if (err < 0) {
		logerr();
		return 1;
	}

	err = close(ip_sock);
	if (err < 0) {
		logerr();
		return 1;
	}
	return 0;
}
