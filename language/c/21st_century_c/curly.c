/*
 * curly.c
 */

#include <stdio.h>
#include <stdlib.h>

// By using the curly braces, the local total variable can override the outside
// total variable. So avoid variables with conflicting names.
#define SUM(max, out)                    \
	{                                    \
		int total = 0;                   \
		for (int i = 0; i <= max; i++) { \
			total += i;                  \
		}                                \
		out = total;                     \
	}

int main(int argc, char *argv[]) {
	int out;
	int total = 5;
	SUM(5, out);
	printf("out= %i original total=%i\n", out, total);
	return EXIT_SUCCESS;
}
