#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

int main() {
	int err;

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	if (sk < 0) {
		logerr();
		return 1;
	}

	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port   = htons(1234),
		.sin_addr   = inet_addr("127.0.0.1"),
	};

	struct sockaddr_in *addr;
	memset(addr, 0, sizeof(addr));

	char *message = "test";

	err = sendto(sk, message, sizeof(message), 0, (struct sockaddr *)addr, sizeof(addr));
	if (err < 0) {
		logerr();
		return 1;
	}

	return 0;
}
