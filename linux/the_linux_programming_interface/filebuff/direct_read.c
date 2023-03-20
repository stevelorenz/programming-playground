/*
 *  direct_read file length [offset [alignment]]
 * */

#define _GNU_SOURCE	 // Obtain the O_DIRECT from fcntl
#include <fcntl.h>
#include <malloc.h>

#include "tlpi_hdr.h"

int main(int argc, char* argv[]) {
	if (argc < 3) {
		usageErr("%s file length [offset [alignment]]\n", argv[0]);
	}

	size_t length, alignment;
	off_t offset;
	length = getLong(argv[2], GN_ANY_BASE, "length");
	offset = (argc > 3) ? getLong(argv[3], GN_ANY_BASE, "offset") : 0;
	alignment = (argc > 4) ? getLong(argv[4], GN_ANY_BASE, "alignment") : 4096;

	int fd;
    fd = open(argv[1], O_RDONLY | O_DIRECT);
	if (fd == -1) {
		errExit("open");
	}
    printf("* After open!");

	char* buf;
	buf = memalign(alignment * 2, length + alignment);
	if (buf == NULL) {
		errExit("memalign");
	}
	buf += alignment;

	if (lseek(fd, offset, SEEK_SET) == -1) {
		errExit("lseek");
	}

	ssize_t numRead;
	numRead = read(fd, buf, length);
	if (numRead == -1) {
		errExit("read");
	}
	printf("Read %ld bytes\n", (long)(numRead));

	close(fd);
	exit(EXIT_SUCCESS);
}
