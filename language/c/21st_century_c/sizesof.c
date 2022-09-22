/*
 * sizesof.c
 */

#include <stdio.h>
#include <stdlib.h>

// When use # inside a macro, it turns the argument into plain string!
#define PEVAL(cmd) printf(#cmd ": %g\n", cmd);

int main(int argc, char *argv[]) {
	double *plist = (double[]){1, 2, 3};
	size_t i = 0;
	for (i = 0; i < 3; ++i) {
		printf("%g, ", *(plist + i));
	}
	printf("\n");
	double list[] = {1, 2, 3};
	PEVAL(sizeof(plist) / (sizeof(double) + 0.0));
	PEVAL(sizeof(*plist) / (sizeof(double) + 0.0));
	PEVAL(sizeof(list) / (sizeof(double) + 0.0));

	return EXIT_SUCCESS;
}
