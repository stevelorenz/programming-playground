#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>
// For S_* constants
#include <sys/stat.h>
// For O_* constants

#define SH_SIZE 16

int main(int argc, char *argv[]) {
	int shm_fd = shm_open("/shm0", O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
	if (shm_fd < 0) {
		fprintf(stderr,
				"Failed to create the shared memory with error code: %s",
				strerror(errno));
		exit(1);
	}
	fprintf(stdout, "Shared memory is create with fd: %d\n", shm_fd);
	// On successful completion of shm_open() returns a new file descriptor
	// referring to the shared memory object.
	if (ftruncate(shm_fd, SH_SIZE * sizeof(char)) < 0) {
		fprintf(stderr, "Failed to truncate the file with error code:%s\n",
				strerror(errno));
		exit(2);
	}
	fprintf(stdout, "The memory region is truncate!\n");

	void *map;
	map = mmap(0, SH_SIZE, PROT_WRITE, MAP_SHARED, shm_fd, 0);
	if (map == MAP_FAILED) {
		fprintf(stderr, "Mapping failed!\n");
		exit(3);
	}

	char *ptr;
	ptr = (char *)map;
	ptr[0] = 'A';
	ptr[1] = 'B';
	ptr[2] = 'C';
	ptr[3] = '\n';
	ptr[4] = '\0';

	if (munmap(ptr, SH_SIZE) < 0) {
		fprintf(stderr, "Failed to unmap: %s\n", strerror(errno));
		exit(4);
	}

	if (close(shm_fd) < 0) {
		fprintf(stderr, "Failed to close the shared memory object: %s\n",
				strerror(errno));
		exit(5);
	}
	return 0;
}
