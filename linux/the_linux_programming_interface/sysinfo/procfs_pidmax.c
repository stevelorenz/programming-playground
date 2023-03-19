#include <assert.h>
#include <fcntl.h>

#include "tlpi_hdr.h"

#define MAX_LINE 100

int main() {
	int fd;
	char line[MAX_LINE];
	ssize_t n;

	fd = open("/proc/sys/kernel/pid_max", O_RDONLY);
	if (fd == -1) {
		errExit("open");
	}

	n = read(fd, line, MAX_LINE);
	if (n == -1) {
		errExit("read");
	}
	assert(n + 1 < MAX_LINE);
	line[n + 1] = '\0';

	printf("Current PID Max: %s\n", line);

	exit(EXIT_SUCCESS);
}
