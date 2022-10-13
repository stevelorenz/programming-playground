/*
 * vectorize.c
 */

#include <stdio.h>
#include <stdlib.h>

#define FN_APPLY(type, fn, ...)                                            \
	{                                                                      \
		void* stopper_for_apply = (int[]){0};                              \
		type** list_for_apply = (type*[]){__VA_ARGS__, stopper_for_apply}; \
		for (int i = 0; list_for_apply[i] != stopper_for_apply; i++) {     \
			fn(list_for_apply[i]);                                         \
		}                                                                  \
	}

#define FREE_ALL(...) FN_APPLY(void, free, __VA_ARGS__);

int main(int argc, char* argv[]) {
	double* x = malloc(sizeof(double) * 10);
	double* y = malloc(sizeof(double) * 100);
	double* z = malloc(sizeof(double) * 1000);

	FREE_ALL(x, y, z);
	return EXIT_SUCCESS;
}
