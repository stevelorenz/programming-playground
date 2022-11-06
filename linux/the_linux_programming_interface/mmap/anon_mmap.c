#ifdef USE_MAP_ANON
#define _BSD_SOURCE
#endif

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/wait.h>

#include "tlpi_hdr.h"

int main() {
	int* addr;

#ifdef USE_MAP_ANON
	// Set the fd to -1 to indicate anonymous mapping
	addr = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE,
				MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	if (addr == MAP_FAILED) {
		errExit("map");
	}

#else
	// /dev/zero is supported only on e.g. Linux
	int fd;
	fd = open("/dev/zero", O_RDWR);
	if (fd == -1) {
		errExit("open");
	}
	addr = mmap(NULL, sizeof(int), PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (addr == MAP_FAILED) {
		errExit("mmap");
	}

	if (close(fd) == -1) {
		errExit("close");
	}
#endif

	*addr = 1;

	switch (fork()) {
		case -1:
			errExit("fork");
			break;

		case 0:	 // Child process
			printf("Child started, value = %d\n", *addr);
			(*addr)++;
			if (munmap(addr, sizeof(int)) == -1) {
				errExit("munmap");
			}

			exit(EXIT_SUCCESS);
			break;

		default:  // Parent process
			if (wait(NULL) == -1) {
				errExit("wait");
			}
			printf("In parent, value = %d\n", *addr);
			if (munmap(addr, sizeof(int)) == -1) {
				errExit("munmap");
			}

			break;
	}

	return 0;
}
