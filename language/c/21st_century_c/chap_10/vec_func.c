#include <stdio.h>
#include <stdlib.h>

/**
 * @ list_for_apply is a pointer to an array of pointers with type.
 */
#define FN_APPLY(type, fn, ...)                                                \
	{                                                                      \
		void *stopper_for_apply = (int[]){ 0 };                        \
		type **list_for_apply =                                        \
			(type *[]){ __VA_ARGS__, stopper_for_apply };          \
		for (int i = 0; list_for_apply[i] != stopper_for_apply; i++)   \
			fn(list_for_apply[i]);                                 \
	}

#define FREE_ALL_DOUBLE(...) FN_APPLY(double, free, __VA_ARGS__)

int main(int argc, char *argv[])
{
	double *x = malloc(10 * sizeof(double));
	double *y = malloc(20 * sizeof(double));
	double *z = malloc(30 * sizeof(double));

	// Use the macro to vectorize the free functions.
	FREE_ALL_DOUBLE(x, y, z);

	return 0;
}
