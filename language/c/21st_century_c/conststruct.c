/*
 * conststruct.c
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
	// Internal pointers are NOT const
	int *counter1;
	int *counter2;
} counter_s;

void check_counter(int *ctr) {
	assert(ctr != 0);
	assert(ctr != NULL);
};

double ratio(counter_s const *in) {
	check_counter(in->counter2);
	return (double)(*(in->counter1)) / (*(in->counter2));
}

int main(int argc, char *argv[]) {
	counter_s cc = {
		.counter1 = malloc(sizeof(int)),
		.counter2 = malloc(sizeof(int)),
	};
	*cc.counter1 = *cc.counter2 = 1;
	ratio(&cc);

	free(cc.counter1);
	free(cc.counter2);
	return EXIT_SUCCESS;
}
