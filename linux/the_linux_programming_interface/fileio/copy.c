#include <fcntl.h>
#include <gnu/libc-version.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>

#include "tlpi_hdr.h"

#ifndef BUF_SIZE
#define BUF_SIZE 1024
#endif

int main(int argc, char const* argv[]) {
	// version is a pointer which points to a constant character
	char const* version = gnu_get_libc_version();
	printf("LibC version: %s\n", version);

	int inputFD, outputFD, openFlags;
	mode_t filePerms;
	ssize_t numRead;
	char buf[BUF_SIZE];

	if (argc != 3 || strcmp(argv[1], "--help") == 0) {
		errExit("%s old-file new-file\n", argv[0]);
	}

	inputFD = open(argv[1], O_RDONLY);
	if (inputFD == -1) {
		errExit("open file %s", argv[1]);
	}

	openFlags = O_CREAT | O_WRONLY | O_TRUNC;
	filePerms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH |
				S_IWOTH; /* rw-rw-rw- */
	outputFD = open(argv[2], openFlags, filePerms);
	if (outputFD == -1) {
		errExit("open file %s", argv[1]);
	}

	while ((numRead = read(inputFD, buf, BUF_SIZE)) > 0) {
		if (write(outputFD, buf, numRead) != numRead) {
			fatal("write() return error or partial write");
		}
	}
	if (numRead == -1) {
		errExit("read");
	}

	if (close(inputFD) == -1) {
		errExit("close input");
	}
	if (close(outputFD) == -1) {
		errExit("close input");
	}

	return EXIT_SUCCESS;
}
