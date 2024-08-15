#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
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
};

struct pseudo_header {
	u32 src_addr;
	u32 dest_addr;
	u8 zero;
	u8 protocol;
	u16 udp_len;
};

u16 checksum(u8 *buf, int len) {
	u32 sum = 0;

	// sum += ntohl(phdr->src_addr);
	// sum += ntohl(phdr->dest_addr);
	// sum += ntohs(phdr->protocol);
	// sum += ntohs(phdr->udp_len);

	// sum += phdr->src_addr;
	// sum += phdr->dest_addr;
	// sum += phdr->protocol;
	// sum += phdr->udp_len;

    for (int i = 0; i < len; i++) {
		if (i & 1) {
			sum += (u32)buf[i];
		} else {
			sum += (u32)buf[i] << 8;
		}
    }

    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    return htons(~sum);

    // sum = ~sum & 0xFFFF;
    // return htons(sum);
}


int main() {
	int err;
	u8 *message = "";

	int sk = socket(AF_INET, SOCK_RAW, IPPROTO_UDP);
	if (sk < 0) goto ERR;

	struct header hdr = {
		.src_port = htons(1234),
		.dest_port = htons(1234),
		.len = htons(strlen(message)+8),
		.checksum = 0, // calculated later
	};

	struct in_addr tmp;
	inet_aton("127.0.0.1", &tmp);

	struct pseudo_header phdr = {
		.src_addr = htonl(tmp.s_addr),
		.dest_addr = htonl(tmp.s_addr),
		// .src_addr = tmp.s_addr,
		// .dest_addr = tmp.s_addr,
		.zero = 0,
		// .protocol = htons(IPPROTO_UDP),
		.protocol = IPPROTO_UDP,
		.udp_len = hdr.len,
	};

	u8 buf[1024] = {0};
	memcpy(buf, &phdr, sizeof(phdr));
	memcpy(buf+sizeof(phdr), message, strlen(message));
	hdr.checksum = checksum(buf, sizeof(phdr)+strlen(message));

	u8 packet[1024] = {0};
	memcpy(packet, &hdr, sizeof(hdr));
	memcpy(packet+sizeof(hdr), message, strlen(message));

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
