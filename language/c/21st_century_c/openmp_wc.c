#include <omp.h>
#include <stdint.h>

#include "stopif.h"
#include "wordcount.c"

int main(int argc, char *argv[]) {
	argc--;
	argv++;

	STOP_IF(!argc, return 0,
			"Please give some file names on the command line.");

	uint64_t count[argc];
	size_t i = 0;

	omp_set_num_threads(omp_get_num_procs());
// This single line let OpenMP spawn the following loop with multiple.threads.
#pragma omp parallel for
	for (i = 0; i < argc; ++i) {
		count[i] = wc(argv[i]);
		printf("%s:\t%llu\n", argv[i], count[i]);
	}

	uint64_t sum = 0;
	for (i = 0; i < argc; ++i) {
		sum += count[i];
	}
	printf("sum: %llu\n", sum);

	return 0;
}
