#define _POSIX_SOURCE

#include <signal.h>
#include <stdio.h>
#include <unistd.h>

int siginterrupt(int sig, int flag) {
	int status;
	struct sigaction act;

	status = sigaction(sig, NULL, &act);
	if (status == -1) {
		return -1;
	}
	// if (flag) {
	// 	act.sa_flags &= ~SA_RESTART;

	// } else {
	// 	act.sa_flags |= SA_RESTART;
	// }

	return sigaction(sig, &act, NULL);
}

int main(int argc, char *argv[]) { return 0; }
