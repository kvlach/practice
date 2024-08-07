#include <errno.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define u8 unsigned char

#define BUFFER_LEN 10

int main() {
	u8 buffer[BUFFER_LEN];

	int ip_sock = socket(AF_INET, SOCK_RAW, 0);
	if (ip_sock < 0)  {
		perror("");
		return 1;
	}

	ssize_t n = recv(ip_sock, buffer, BUFFER_LEN, 0);

	printf("%d\n", n);
	return 0;
}
