#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#define u32 uint32_t
#define u64 uint64_t
#define byte unsigned char

typedef struct item {
	byte val;
	struct item *next;
} item;

item *new_item(byte val) {
	item *it = (item *)malloc(sizeof(item));
	it->val = val;
	it->next = NULL;
	return it;
}

typedef struct list {
	u32 len;
	item *head;
} list;

list *new_list() {
	list *ls = (list *)malloc(sizeof(list));
	ls->len = 0;
	ls->head = NULL;
	return ls;
}

void list_free(list *ls) {
	item *it = ls->head;
	while (it) {
		item *tmp = it;
		it = it->next;
		free(tmp);
	}
	free(ls);
}

item *list_tail(list *ls) {
	item *it = ls->head;
	while(it->next) {
		it = it->next;
	}
	return it;
}

void list_append(list *ls, byte b) {
	item *it = new_item(b);

	if (ls->len == 0) {
		ls->head = it;
	} else {
		item *tail = list_tail(ls);
		tail->next = it;
	}

	ls->len++;
}

void list_append_str(list *ls, char *str) {
	while (*str) {
		list_append(ls, *str);
		str++;
	}
}

void list_append_u64(list *ls, u64 n) {
	list_append(ls, n&0xff);
	list_append(ls, (n>>8)&0xff);
	list_append(ls, (n>>16)&0xff);
	list_append(ls, (n>>24)&0xff);
	list_append(ls, (n>>32)&0xff);
	list_append(ls, (n>>40)&0xff);
	list_append(ls, (n>>48)&0xff);
	list_append(ls, (n>>56)&0xff);
	// list_append(ls, (n>>56)&0xff);
	// list_append(ls, (n>>48)&0xff);
	// list_append(ls, (n>>40)&0xff);
	// list_append(ls, (n>>32)&0xff);
	// list_append(ls, (n>>24)&0xff);
	// list_append(ls, (n>>16)&0xff);
	// list_append(ls, (n>>8)&0xff);
	// list_append(ls, n&0xff);
}

void list_print(list *ls) {
	item *it = ls->head;

	printf("len=%d, list=", ls->len);
	while (it) {
		printf("%02x ", it->val);
		it = it->next;
	}
	puts("");
}

u32 list_read_u32(list *ls, int start) {
	item *it = ls->head;
	for (int i = 1; i < start; i++) {
		it = it->next;
	}

	u32 a = it->val;
	it = it->next;

	u32 b = it->val;
	it = it->next;

	u32 c = it->val;
	it = it->next;

	u32 d = it->val;

	// return (a << 24) | (b << 16) | (c << 8) | d;
	return (d << 24) | (c << 16) | (b << 8) | a;
}

u32 left_rotate(u32 x, u32 k) {
	return (x<<k) | (x>>(32-k));
}

void print_hex(u32 n) {
	printf("%02x%02x%02x%02x", n&0xff, (n>>8)&0xff, (n>>16)&0xff, (n>>24)&0xff);
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

	list *message = new_list();
	// list_append_str(message, "test ");
	// list_append_str(message, "");
	list_append_str(message, "The quick brown fox jumps over the lazy dog");
	u64 original_len_bits = message->len * 8;

	list_append(message, 0x80);
	while (message->len % 64 != 56) {
		list_append(message, 0x0);
	}
	list_append_u64(message, original_len_bits);

	list_print(message);

	u32 i, j;

	for (j = 0; j < message->len; j+=64) {
		u32 M[16];

		for (i = 0; i < 16; i++) {
			M[i] = list_read_u32(message, j+i*4);
			// printf("%u ", M[i]);
			printf("%d\n", j+i*4);
		}
		puts("");

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

	// printf("%08x%08x%08x%08x\n", a0, b0, c0, d0);
	print_hex(a0);
	print_hex(b0);
	print_hex(c0);
	print_hex(d0);
	puts("");

	return 0;
}
