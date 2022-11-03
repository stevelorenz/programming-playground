#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void handle_user_signals(int signal) {
	switch (signal) {
		case SIGUSR1:
			printf("SIGUSR1 is received.\n");
			break;
		case SIGUSR2:
			printf("SIGUSR2 is received.\n");
			break;
		default:
			printf("Unsupported signal is received.\n");
	}
}

// SIGKILL can not be handled with any process.
void handle_sigkill(int signal) {
	printf("Kill signal is received. Bye!\n");
	exit(0);
}

int main(int argc, char *argv[]) {
	signal(SIGUSR1, handle_user_signals);
	signal(SIGUSR2, handle_user_signals);
	signal(SIGKILL, handle_sigkill);
	while (1) {
		usleep(1000);
	}
	return 0;
}
