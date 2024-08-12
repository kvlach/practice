#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#define u8 unsigned char
#define u16 uint16_t
#define u32 uint32_t

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
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
	return 0;
};

int main() {
	char *message = "test";
	int err;

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
	if (sk < 0) {
		goto ERR;
	}

	struct in_addr addr;
	inet_aton("127.0.0.1", &addr);

	struct sockaddr_in dest_addr = {
		.sin_family = AF_INET,
		.sin_port = htons(1234),
		.sin_addr.s_addr = addr.s_addr,
	};

	struct header hdr = {
		.src_port = htons(1234),
		.dest_port = htons(1234),
		.len = strlen(message)+8,
		.checksum = checksum(),
		.data = message,
	};

	struct pseudo_header phdr = {
		.src_addr = addr.s_addr,
		.dest_addr = addr.s_addr,
		.zero = 0,
		.protocol = 0,
		.udp_len = strlen(message)+8,
	};

	u8 packet[1024] = {0};
	memset(packet, &hdr, sizeof(hdr));
	memset(packet+sizeof(hdr), message, strlen(message));

	err = sendto(sk, packet, sizeof(hdr)+strlen(message), 0, (struct sockaddr *)&dest_addr, sizeof(dest_addr));
	if (err < 0) {
		goto ERR;
	}

	close(sk);
	return 0;
ERR:
	if (sk > 0) close(sk);
	logerr();
	return 1;
}
