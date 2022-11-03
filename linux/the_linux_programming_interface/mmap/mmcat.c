#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>

#include "error_functions.h"
#include "tlpi_hdr.h"

int main(int argc, char *argv[]) {
	if (argc != 2 || strcmp(argv[1], "--help") || strcmp(argv[1], "-h")) {
		usageErr("%s file\n", argv[0]);
	}

	return 0;
}
