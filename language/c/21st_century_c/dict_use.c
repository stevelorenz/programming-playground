#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "dict.h"

int main(int argc, char *argv[]) {
	dictionary *d = dictionary_new();

	int zero = 0;
	float one = 1.0;
	char two[] = "two";

	dictionary_add(d, "an int", &zero);
	dictionary_add(d, "a float", &one);
	dictionary_add(d, "a string", &two);

	int zero_found = *((int *)(dictionary_find(d, "an int")));
	assert(zero == zero_found);

	dictionary_free(d);
	return EXIT_SUCCESS;
}
