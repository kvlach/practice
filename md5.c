#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define u32 uint32_t
#define byte unsigned char

void print_hex(byte *str) {
	while (*str) {
		printf("%02x ", *str);
		str++;
	}
	puts("");
}

typedef list struct {
	size_t len;
	byte *start;
} list;

list *new_list() {
	list *ls = (list *)malloc(sizeof(list));
	ls->len = 0;
	ls->start = NULL;
	return ls;
}

list_append(list *ls, byte b) {
	
}

void str_append_u32(byte *str, u32 n) {
	printf("'%s'\n", str);
	print_hex(str);
	int len = strlen(str);
	printf("'%c'\n", str[len-2]);
	printf("%02x\n", str[len]);
	str[len] = (n >> 24) & 0xff;
	printf("%02x\n", str[len]);
	str[len+1] = (n >> 16) & 0xff;
	str[len+2] = (n >> 8) & 0xff;
	str[len+3] = n & 0xff;
	str[len+4] = '\0';
	print_hex(str);
	printf("'%s'\n", str);
}

u32 str_to_u32(byte *str) {
	return *(str) << 24 | *(str+8) << 16 | *(str+16) << 8 | *(str+24);
}

u32 left_rotate(u32 x, u32 k) {
	return (x<<k) | (x>>(32-k));
}

int main() {
	u32 s[64] = {
		7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
		5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
		4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
		6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,
	};

	u32 K[64] = {
		0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
		0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
		0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
		0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
		0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
		0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
		0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
		0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
		0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
		0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
		0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
		0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
		0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
		0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
		0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
		0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391,
	};

	u32 a0 = 0x67452301;
	u32 b0 = 0xefcdab89;
	u32 c0 = 0x98badcfe;
	u32 d0 = 0x10325476;

	byte *message = (byte *)malloc(100*sizeof(byte));
	memcpy(message, "test test", sizeof("test test"));
	u32 original_len_bits = strlen(message) * 8;

	printf("'%s'\n", message);
	str_append_u32(message, 0x80);
	printf("'%s'\n", message);
	return 0;
	while (strlen(message)%64 != 56) {
		str_append_u32(message, 0x0);
	}
	str_append_u32(message, original_len_bits);

	u32 i, j;
	for (j = 0; j < strlen(message); j+=64) {
		u32 M[16];
		for (i = 0; i < 16; i++) {
			M[i] = str_to_u32(message+j+i*4);
		}

		u32 A = a0;
		u32 B = b0;
		u32 C = c0;
		u32 D = d0;

		for (i = 0; i < 64; i++) {
			u32 F, g;

			if (0 <= i && i <= 15) {
				F = (B & C) | (~B & D);
				g = i;
			} else if (16 <= i && i <= 31) {
				F = (D & B) | (~D & C);
				g = (5*i + 1) % 16;
			} else if (32 <= i && i <= 47) {
				F = B ^ C ^ D;
				g = (3*i + 5) % 16;
			} else {
				F = C ^ (B | ~D);
				g = (7*i) % 16;
			}

			F += A + K[i] + M[g];
			A = D;
			D = C;
			C = B;
			B += left_rotate(F, s[i]);
		}

		a0 += A;
		b0 += B;
		c0 += C;
		d0 += D;
	}

	byte digest[16];

	str_append_u32(digest, a0);
	str_append_u32(digest, b0);
	str_append_u32(digest, c0);
	str_append_u32(digest, d0);

	printf("%x\n", digest);

	free(message);
	return 0;
}
