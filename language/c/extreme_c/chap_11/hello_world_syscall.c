/*
 * syscall_direct.c
 * About: Use my amazing hello world syscall.
 */

#define _GNU_SOURCE

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/syscall.h>

int main(int argc, char *argv[])
{
	char str[20] = "Zuo";
	char message[64] = "";
	// Invoke the write system call to write bytes into standard output.
	// 15 is the length of the string.
	if (syscall(999, str, 4, message, 64) == -1) {
		fprintf(stderr, "Fail to invoke the hello_world syscall!");
		exit(1);
	}

	printf("Message: %s\n", message);
	return 0;
}
