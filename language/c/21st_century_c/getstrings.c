/*
 * getstrings.c
 */

#define _GNU_SOURCE	 // cause stdio.h to include asprintf

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>	 //free

void get_strings(char const *in) {
	char *cmd = NULL;
	asprintf(&cmd, "strings %s", in);
	printf("%s\n", cmd);
	free(cmd);
}

int main(int argc, char *argv[]) {
	assert(argc == 2);
	get_strings(argv[1]);
	return EXIT_SUCCESS;
}
