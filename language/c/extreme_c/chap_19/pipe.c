#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
	int pipefd[2];
	assert(pipe(pipefd) != -1);

	pid_t childpid = fork();
	assert(childpid != -1);

	if (childpid == 0) {
		// Child process writes into the pipe.
		printf("Child: Pipefd:%d,%d\n", pipefd[0], pipefd[1]);
		close(pipefd[0]);
		char str[] = "Hello, Daddy!";
		fprintf(stdout, "Child: wait for 2 seconds...\n");
		sleep(2);
		fprintf(stdout, "Child: write data into the pipe...\n");
		write(pipefd[1], str, strlen(str) + 1);

	} else {
		// Parent process reads from the pipe.
		printf("Parent: Pipefd:%d,%d\n", pipefd[0], pipefd[1]);
		close(pipefd[1]);
		char buf[32];
		fprintf(stdout, "Parent: read from the pipe...\n");
		read(pipefd[0], buf, 32);
		fprintf(stdout, "Parent: received from the pipe: %s\n", buf);
	}

	return 0;
}
