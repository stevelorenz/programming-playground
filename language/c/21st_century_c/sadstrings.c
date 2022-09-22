/*
 * sadstrings.c
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void get_strings(char const *in) {
	char *cmd;
	int len = strlen("strings ") + strlen(in) + 1;
	cmd = malloc(len * sizeof(char));
	snprintf(cmd, len, "strings %s", in);
	printf("%s\n", cmd);
	free(cmd);
}

int main(int argc, char *argv[]) {
	assert(argc == 2);
	get_strings(argv[1]);
	return EXIT_SUCCESS;
}
