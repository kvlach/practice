#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <arpa/inet.h>

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
	u32 udp_len;
};

u16 checksum(struct pseudo_header *phdr, int len) {
	u32 sum = 0;
	sum += phdr->src_addr;
	sum += phdr->dest_addr;
	sum += phdr->protocol;
	sum += phdr->udp_len;
	return 0;
}

int main() {
	int err;
	char *message = "test";

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
	if (sk < 0) goto ERR;

	struct in_addr tmp;
	inet_aton("127.0.0.1", &tmp);

	struct pseudo_header phdr {
		.src_addr = tmp.s_addr,
		.dest_addr = tmp.s_addr,
		.zero = 0,	
		.protocol = htons(IPPROTO_UDP),
		.udp_len = htons(strlen(message)+8),	
	};

	struct header hdr = {
		.src_port = htons(1234),
		.dest_port = htons(1234),
		.len = phdr.udp_len,
		.checksum = 0,
		.data = message,
	};

	hdr.checksum = checksum();

	u8 packet[1024] = {0};
	memcpy(packet, &hdr, hdr.len);

	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port = htons(1234),
		.sin_addr = tmp,
	};

	err = sendto(sk, packet, hdr.len, 0, (struct sockaddr *)&addr, sizeof(addr));
	if (err < 0) goto ERR;

	close(sk);
	return 0;
ERR:
	if (sk > 0) close(sk);
	logerr();
	return 1;
}
