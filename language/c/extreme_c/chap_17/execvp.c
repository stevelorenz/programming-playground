#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
	int ret = 0;
	char *args[] = {"echo", "hello", "world!", NULL};

	ret = execvp("echo", args);
	if (ret) {
		printf("execvp faild with error number: %s\n", strerror(errno));
		exit(1);
	}
	return 0;
}
