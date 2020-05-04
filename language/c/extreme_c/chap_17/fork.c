#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
	printf("This is the parent process with PID: %d\n", getpid());
	pid_t ret;

	ret = fork();
	if (ret == -1) {
		printf("Failed to fork the process.\n");
		exit(1);
	}
	if (ret) {
		printf("The parent process created a child process with PID: %d\n",
		       ret);
	} else {
		printf("This is the child process with PID:%d\n", getpid());
	}

	return 0;
}
