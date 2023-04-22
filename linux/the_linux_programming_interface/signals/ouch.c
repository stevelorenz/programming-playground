#include <signal.h>

#include "tlpi_hdr.h"

static void sigHandler(int sig) {
	printf("Ouch!\n");	// Unsafe !
	return;
}

int main() {
	int i;

	if (signal(SIGINT, sigHandler) == SIG_ERR) {
		errExit("signal");
	}

	for (i = 0;; ++i) {
		printf("%d\n", i);
		sleep(3);
	}

	exit(EXIT_SUCCESS);
}
