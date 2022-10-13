/*
 * varad.c
 */

#include <stdio.h>
#include <stdlib.h>

#define forloop(i, loopmax, ...)        \
	for (int i = 0; i < loopmax; i++) { \
		__VA_ARGS__                     \
	}

int main(int argc, char *argv[]) {
	int sum = 0;
	forloop(i, 10, sum += i; printf("Sum is %d\n", sum););
	return EXIT_SUCCESS;
}
