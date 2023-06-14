#include <signal.h>
#include <stdio.h>
#include <unistd.h>

#include "error_functions.h"
#include "tlpi_hdr.h"

static void sigHandler(int sig) {
	static int count = 0;

	if (sig == SIGINT) {
		count++;
		printf("Caught SIGINT (%d)\n", count);
		if (signal(SIGINT, sigHandler) == SIG_ERR) {
			errExit("signal");
		}
		return;
	}

	printf("Caught SIGQUIT - that's all folks\n");
	exit(EXIT_SUCCESS);
}

int main() {
	// The callback of signal ONLY runs once. You need to register callback
	// again inside the callback function to handle signal multiple times.
	// However, there's a race condition here. So better use sigaction.
	if (signal(SIGINT, sigHandler) == SIG_ERR) {
		errExit("signal");
	}
	if (signal(SIGQUIT, sigHandler) == SIG_ERR) {
		errExit("signal");
	}

	for (;;) {
		pause();  // Block until a signal is caught
	}
}
