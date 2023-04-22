#define _POSIX_SOURCE

#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	if (argc != 3 || strcmp(argv[1], "--help") == 0) {
		usageErr("%s pid sig-num\n", argv[0]);
	}

	int sig = 0;
	sig = getInt(argv[2], 0, "sig-num");

	int s;
	s = kill(getLong(argv[1], 0, "pid"), sig);

	exit(EXIT_SUCCESS);
}
