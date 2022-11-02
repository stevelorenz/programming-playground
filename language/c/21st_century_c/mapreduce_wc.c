#include <omp.h>
#include <stdint.h>

#include "stopif.h"
#include "wordcount.c"

int main(int argc, char *argv[]) {
	argc--;
	argv++;

	STOP_IF(!argc, return 0,
			"Please give some file names on the command line.");

	omp_set_num_threads(omp_get_num_procs());

	uint64_t tmp;
	size_t i = 0;
	uint64_t sum = 0;

#pragma omp parallel for reduction(+ : sum)
	for (i = 0; i < argc; ++i) {
		tmp = wc(argv[i]);
		sum += tmp;
		printf("%s:\t%llu\n", argv[i], tmp);
	}
	printf("sum: %llu\n", sum);

	return 0;
}
