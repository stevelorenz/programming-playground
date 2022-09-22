/*
 * sasprintf.c
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>

// The write_to pointer will be assigned with a new address with asprintf.
// Then the previous string is not freed, so a temporary pointer is used to
// store the address of the original string, then automatically free it after
// reallocating.
#define SASPRINTF(write_to, ...)            \
	{                                       \
		char *tmp = (write_to);             \
		asprintf(&(write_to), __VA_ARGS__); \
		free(tmp);                          \
	}

int main(int argc, char *argv[]) {
	char *q = NULL;
	SASPRINTF(q, "select * from tab");
	int i = 3;
	SASPRINTF(q, "%s where col%i is not null", q, i);
	printf("%s\n", q);
	free(q);

	return EXIT_SUCCESS;
}
