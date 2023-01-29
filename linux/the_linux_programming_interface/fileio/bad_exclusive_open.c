#include <fcntl.h>
#include <sys/fcntl.h>
#include <sys/stat.h>

#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	int fd;
	if (argc < 2 || strcmp(argv[1], "--help") == 0) {
		usageErr("%s file\n", argv[0]);
	}

	fd = open(argv[1], O_WRONLY);
	if (fd != -1) {
		printf("[PID %ld] File \"%s\" already exists\n", (long)getpid(),
			   argv[1]);
		close(fd);
	} else {
		if (errno != ENOENT) {
			errExit("open");
		} else {
			printf("[PID %ld] File \"%s\" doesn't exist yet\n", (long)getpid(),
				   argv[1]);
			if (argc > 2) {
				// Long sleep to "make" the CPU scheduler switch between process
				// This context switch may trigger a race condition.
				sleep(5);
				printf("[PID %ld] Done sleeping\n", (long)getpid());
			}
			fd = open(argv[1], O_WRONLY | O_CREAT, S_IRUSR, S_IWUSR);
			if (fd == -1) {
				errExit("open");
			}
			printf("[PID %ld] Created file \"%s\" exclusively\n",
				   (long)getpid(), argv[1]); /* MAY NOT BE TRUE! */
		}
	}

	exit(EXIT_SUCCESS);
}
