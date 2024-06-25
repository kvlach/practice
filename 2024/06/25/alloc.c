#include <sys/mman.h>
#include <string.h>
#include <stdio.h>

void *alloc(size_t len) {
	void *addr = mmap(0, len, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
	// save the size of the allocated memory at the beginning and then increment
	*(size_t *)addr = len;
	return (size_t *)addr + 1;
}

void free(void *addr) {
	addr = (size_t *)addr - 1;
	size_t len = *(size_t *)(addr);
	munmap(addr, len);
}

int main() {
	char *str = alloc(20);
	strcat(str, "test");
	strcat(str, "1234test");
	printf("%s\n", str);
	free(str);
	printf("%s\n", str);

	return 0;
}
