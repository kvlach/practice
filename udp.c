#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

#define u8 unsigned char
#define u16 uint16_t
#define u32 uint32_t

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

void debug_addr(struct sockaddr_in addr) {
	printf("family=%d port=%d addr=%d\n", addr.sin_family, addr.sin_port, addr.sin_addr);
}

struct header {
	u16 src_port;
	u16 dest_port;
	u16 len;
	u16 checksum;
	u8 *data;
};

struct pseudo_header {
	u32 src_addr;
	u32 dest_addr;
	u8 zero;
	u8 protocol;
	u16 udp_len;
};

u16 checksum() {
	// TODO
	return 0;	
}

int main() {
	int err;
	char *message = "test";

	// int sk = socket(AF_INET, SOCK_RAW, IPPROTO_RAW);
	int sk = socket(AF_INET, SOCK_DGRAM, 0);
	if (sk < 0) {
		goto ERR;
	}

	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port   = htons(1234),
		// .sin_port   = 1234,
	};
	err = inet_aton("127.0.0.1", &addr.sin_addr);
	if (err < 0) {
		goto ERR;
	}
	debug_addr(addr);

	struct header hdr = {
		.src_port = 1234,
		.dest_port = 1234,
		.len = strlen(message) + 8, // header fields are 8 bytes
		.checksum = checksum(),
		.data = message,
	};

	// struct pseudo_header phdr = {
	// 	.src_addr = ,
	// 	.dest_addr = ,
	// 	.zero = 0,
	// 	.protocol = ,
	// 	.udp_len = strlen(message) + 8,
	// };

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
