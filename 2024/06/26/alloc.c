#include <sys/mman.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

void *alloc(size_t len) {
	len += sizeof(size_t);
	void *addr = mmap(0, len, PROT_READ|PROT_WRITE, MAP_ANON|MAP_PRIVATE, -1, 0);
	if (errno != 0) {
		return NULL;
	}
	*(size_t *)addr = len;
	return (size_t *)addr + 1;
}

void free(void *addr) {
	addr = (size_t *)addr - 1;
	size_t len = *(size_t *)addr;
	munmap(addr, len);
}

int main() {
	char *str = alloc(1);
	if (str == NULL) {
		return 1;
	}

	strcat(str, "test");
	strcat(str, "123");
	printf("%s\n", str);

	free(str);
	printf("\n%s\n", str);

	return 0;
}
