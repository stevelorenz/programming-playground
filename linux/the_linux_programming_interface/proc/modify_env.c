#include "error_functions.h"
#define _GNU_SOURCE
#include <stdlib.h>

#include "tlpi_hdr.h"

// It's defined by the C runtime
extern char **environ;

int main(int argc, char *argv[]) {
    // MARK: clearenv is not portable, GNU only.
	clearenv();

	int j;

	for (j = 1; j < argc; j++) {
		if (putenv(argv[j]) != 0) {
			errExit("putenv :%s", argv[j]);
		}
	}

	if (setenv("GREET", "Hello World", 0) == -1) {
		errExit("setenv");
	}

	unsetenv("BYE");

	char **ep;

	// Print all environ variables
	for (ep = environ; *ep != NULL; ep++) {
		puts(*ep);
	}

	exit(EXIT_SUCCESS);
}
