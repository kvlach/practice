#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <arpa/inet.h>

#define u8 unsigned char
#define u16 uint16_t
#define u32 uint32_t

#define PORT 1234
#define ADDRESS 127.0.0.1

struct header {
	u16 src_port;
	u16 dest_port;
	u16 len;
	u16 checksum;
};

struct pseudo_header {
	u32 src_addr;
	u32 dest_addr;
	u8 zero;
	u8 protocol;
	u16 udp_len;
};

void logerr() {
	fprintf(stderr, "%d: %s\n", errno, strerror(errno));
}

u16 checksum(struct pseudo_header *phdr, struct header *hdr, char *data) {
	u32 sum = 0;

	sum += (phdr->src_addr >> 16) & 0xFFFF;
	sum += (phdr->src_addr & 0xFFFF);
	sum += (phdr->dest_addr >> 16) & 0xFFFF;
	sum += (phdr->dest_addr & 0xFFFF);
	sum += htons(phdr->protocol);
	sum += htons(phdr->udp_len);

	sum += hdr->src_port;
	sum += hdr->dest_port;
	sum += hdr->len;

	u16 len = strlen(data);
	for (int i = 0; i < len-1; i += 2) {
		sum += (data[i] << 8) + (i+1 < len ? data[i+1] : 0);
	}

	while (sum >> 16) {
		sum = (sum & 0xFFFF) + (sum >> 16);
	}

	return (u16)~sum;
}


int main() {
	char *message = "test";
	int err;

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
	if (sk < 0) goto ERR;

	struct header hdr = {
		.src_port = htons(PORT),
		.dest_port = htons(PORT),
		.len = htons(strlen(message)+8),
		.checksum = 0,
	};

	struct in_addr tmp;
	inet_aton("127.0.0.1", &tmp);

	struct pseudo_header phdr = {
		.src_addr = tmp.s_addr,
		.dest_addr = tmp.s_addr,
		.zero = 0,
		.protocol = IPPROTO_UDP,
		.udp_len = hdr.len,
	};

	hdr.checksum = checksum(&phdr, &hdr, message);

	u8 packet[1024];
	memcpy(packet, &hdr, sizeof(hdr));
	memcpy(packet+sizeof(hdr), message, strlen(message));

	struct sockaddr_in addr = {
		.sin_family = AF_INET,
		.sin_port = htons(PORT),
		.sin_addr = tmp,
	};

	err = sendto(sk, packet, ntohs(hdr.len), 0, (struct sockaddr *)&addr, sizeof(addr));
	if (err < 0) goto ERR;

	close(sk);
	return 0;
ERR:
	if (sk > 0) close(sk);
	logerr();
	return 1;
}
