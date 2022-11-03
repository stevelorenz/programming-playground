#include <stdio.h>
#include <stdlib.h>

#include "process_dir.h"

void print_dir(filestruct in) {
	for (int i = 0; i < in.depth - 1; i++) {
		printf("    ");
	}
	printf("├ %s\n", in.name);
	for (int i = 0; i < in.depth - 1; i++) {
		printf("    ");
	}
	printf("└───┐\n");
}

void print_file(filestruct in) {
	for (int i = 0; i < in.depth; ++i) {
		printf("    ");
	}
	printf("| %s\n", in.name);
}

int main(int argc, char *argv[]) {
	printf("Print the tree of current directory");
	process_dir(.name = ".", .directory_action = print_dir,
				.file_action = print_file);
	return EXIT_SUCCESS;
}
