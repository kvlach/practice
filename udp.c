#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <stdint.h>

#define u8  unsigned byte
#define u16 uint16_t
#define u32 uint32_t

struct header {
	u16 source_port;
	u16 destination_port;
	u16 length;
	u16 checksum;
	u8 *data;
};

struct pseudo_header {
	u32 source_address;
	u32 destination_address;
	u8 zero;
	u8 protocol;
	u16 udp_length;
};

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

int main() {
	int err;
	char *message = "test";

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	if (sk < 0) {
		goto ERR;
	}

	struct in_addr tmp;
	inet_aton("127.0.0.1", &tmp);
	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port   = 1234,
		.sin_addr   = tmp,
	};

	header h = {
		.source_port = 1234,
		.destination_port = 1234,
		.length = strlen(message),

	};

	err = sendto(sk, message, strlen(message), 0, (struct sockaddr *)&addr, sizeof(addr));
	if (err < 0) {
		goto ERR;
	}

	close(sk);
	return 0;
ERR:
	logerr();
	close(sk);
	return 1;
}
