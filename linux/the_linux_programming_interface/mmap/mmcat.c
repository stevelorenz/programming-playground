#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include "error_functions.h"
#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	if ((argc != 2) || (strcmp(argv[1], "--help") == 0) ||
		(strcmp(argv[1], "-h") == 0)) {
		usageErr("%s file\n", argv[0]);
	}

	int fd;
	char *addr;
	struct stat sb;

	fd = open(argv[1], O_RDONLY);
	if (!fd) {
		errExit("open");
	}

	if (fstat(fd, &sb) == -1) {
		errExit("fstat");
	}

	// mmap on a file with zero size is an error
	if (sb.st_size == 0) {
		exit(EXIT_SUCCESS);
	}

	// Let the system pick the addr with NULL given as the first parameter.
	addr = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (addr == MAP_FAILED) {
		errExit("mmap");
	}

	if (write(STDOUT_FILENO, addr, sb.st_size) != sb.st_size) {
		fatal("partical read/write error");
	}

	close(fd);
	munmap(addr, sb.st_size);

	exit(EXIT_SUCCESS);
}
