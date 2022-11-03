/*
 * syscall_direct.c
 * About: An example that invokes the write system call directly (Instead of
 * using the libc interface: printf).
 */

#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <sys/syscall.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
	char message[20] = "Hello syscall!\n";
	// Invoke the write system call to write bytes into standard output.
	// 15 is the length of the string.
	if (syscall(__NR_write, 1, message, 15) == -1) {
		fprintf(stderr, "Fail to invoke the write syscall!");
		exit(1);
	}

	return 0;
}
