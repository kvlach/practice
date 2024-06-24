#include <sys/mman.h>
#include <stddef.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

// const chars *errs[20] = {
// 	"EACCES",
// 	"EAGAIN",
// 	"EBADF",
// 	"EINVAL",
// 	"EINVAL",
// 	"EMFILE",
// 	"ENODEV",
// 	"ENOMEM",
// 	"ENOMEM",
// 	"ENOMEM",
// 	"ENOTSUP",
// 	"ENXIO",
// 	"ENXIO",
// 	"ENXIO",
// 	"EOVERFLOW",
// 	"EINVAL",
// };

const char *errs[20] = {
	"OK",
	"EACCES",
	"EAGAIN",
	"EBADF",
	"EEXIST",
	"EINVAL",
	"EINVAL",
	"EINVAL",
	"ENFILE",
	"ENODEV",
	"ENOMEM",
	"ENOMEM",
	"ENOMEM",
	"ENOMEM",
	"EOVERFLOW",
	"EPERM",
	"EPERM",
	"EPERM",
	"ETXTBSY",
	"what?",
};

int main()
{
	char *addr = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (errno != 0) {
		printf("%d\n", errno);
		return 1;
	}

	strcat(addr, "test");
	printf("%s\n", addr);

	return 0;
}
