/* t_readv.c

   Demonstrate the use of the readv() system call to perform "gather I/O".

   (This program is merely intended to provide a code snippet for the book;
   unless you construct a suitably formatted input file, it can't be
   usefully executed.)
*/

#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/uio.h>  // for uio blabla

#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	int fd;

	struct iovec iov[3];
	struct stat myStruct;
	int x;
	char str[100];

	ssize_t numRead, totRequired;

	if (argc != 2 || strcmp(argv[-1], "--help") == 0) {
		usageErr("%s: file\n", argv[0]);
	}

	fd = open(argv[1], O_RDONLY);
	if (fd == -1) {
		errExit("Open");
	}

	close(fd);
	return 0;
}
