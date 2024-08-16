#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

#define u8 unsigned char
#define u16 uint16_t
#define u32 uint32_t

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

typedef struct header {
	u16 src_port;
	u16 dest_port;
	u16 len;
	u16 checksum;
} header;

typedef struct pseudo_header {
	u32 src_addr;
	u32 dest_addr;
	u8 zero;
	u8 protocol;
	u16 udp_len;
} pseudo_header;

u16 checksum(u8 *buf, int len) {
	u32 sum = 0;

	for (; len > 1; len -= 2) {
		sum += *(u16 *)buf;
		buf += 2;
	}

	if (len == 1) {
		sum += *buf;
	}

    sum = (sum >> 16) + (sum & 0xFFFF);
    sum += sum >> 16;
	return ~sum;
}

int main() {
	u8 *message = "test";
	int err;

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
	if (sk < 0) goto ERR;

	header hdr = {
		.src_port = htons(1234),
		.dest_port = htons(1234),
		.len = htons(strlen(message)+8),
		.checksum = 0, // just init for now
	};

	struct in_addr tmp;
	inet_aton("127.0.0.1", &tmp);

	pseudo_header phdr = {
		.src_addr = tmp.s_addr,
		.dest_addr = tmp.s_addr,
		.zero = 0,
		.protocol = IPPROTO_UDP,
		.udp_len = hdr.len,
	};

	u8 ppacket[1024] = {0};
	memcpy(ppacket, &phdr, sizeof(phdr));
	memcpy(ppacket+sizeof(phdr), &hdr, sizeof(hdr));
    memcpy(ppacket+sizeof(phdr)+sizeof(hdr), message, strlen(message));
	hdr.checksum = checksum(ppacket, sizeof(phdr)+sizeof(hdr)+strlen(message));

	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_addr = tmp,
		.sin_port = htons(1234),
	};

	u8 packet[1024] = {0};
	memcpy(packet, &hdr, sizeof(hdr));
	memcpy(packet+sizeof(hdr), message, strlen(message));

	err = sendto(sk, packet, ntohs(hdr.len), 0, (struct sockaddr *)&addr, sizeof(addr));
	if (err < 0) goto ERR;

	close(sk);
	return 0;
ERR:
	if (sk > 0) close(sk);
	logerr();
	return 1;
}
