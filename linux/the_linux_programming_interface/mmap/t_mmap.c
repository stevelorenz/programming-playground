#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>

#include "error_functions.h"
#include "tlpi_hdr.h"

#define MEM_SIZE 10

int main(int argc, char *argv[]) {
	char *addr;
	int fd;

	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s file [new-value]\n", argv[0]);

	fd = open(argv[1], O_RDWR);
	if (fd == -1) {
		errExit("open");
	}

	addr = mmap(NULL, MEM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (addr == MAP_FAILED) {
		errExit("mmap");
	}
	if (close(fd) == -1) {
		errExit("close");
	}
	printf("Current string=%.*s\n", MEM_SIZE, addr);

	if (argc > 2) {
		if (strlen(argv[2]) >= MEM_SIZE) {
			cmdLineErr("'new-value' is too large");
		}
		// The write operation need the sync operation
		memset(addr, 0, MEM_SIZE);
		// I really hate the C-style string processing...
		strncpy(addr, argv[2], MEM_SIZE - 1);
		// Make sure the modified pages are written back to the disk
		if (msync(addr, MEM_SIZE, MS_SYNC) == -1) {
			errExit("msync");
		}
		printf("Copied \"%s\" to shared memory\n", argv[2]);
	}

	exit(EXIT_SUCCESS);
}
