/*
 * preprocess.c
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

//
#define SETUP_LIST(name, ...)                           \
	double *name##_list = (double[]){__VA_ARGS__, NAN}; \
	int name##_len = 0;                                 \
	while (!isnan(name##_list[name##_len])) {           \
		name##_len++;                                   \
	}

int main(int argc, char *argv[]) {
	SETUP_LIST(items, 1, 2, 4, 8);
	// Print using NAN
	double *p = items_list;
	while (!isnan(*p)) {
		printf("%g, ", *p);
		p++;
	}
	printf("\n");

	// Print using name_len
	int i;
	for (i = 0; i < items_len; ++i) {
		printf("%g, ", items_list[i]);
	}
	printf("\n");

	return 0;
}
