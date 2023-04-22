#define _POSIX_SOURCE

#include <signal.h>

#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	if (argc < 4 || strcmp(argv[1], "--help") == 0)
		usageErr("%s pid num-sigs sig-num [sig-num-2]\n", argv[0]);

	int numSigs, sig;
	pid_t pid;

	pid = getLong(argv[1], 0, "PID");
	numSigs = getInt(argv[2], GN_GT_0, "num-sigs");
	sig = getInt(argv[3], 0, "sig-num");

	printf("%s: sending siginal %d to process %ld for %d times\n", argv[0], sig,
		   (long)pid, numSigs);

	int i = 0;
	for (i = 0; i < numSigs; ++i) {
		if (kill(pid, sig) == -1) {
			errExit("kill");
		}
	}

	if (argc > 4) {
		if (kill(pid, getInt(argv[4], 0, "sig-num-2")) == -1) {
			errExit("kill");
		}
	}
	printf("%s: existing\n", argv[0]);

	exit(EXIT_SUCCESS);
}
