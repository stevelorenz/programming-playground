#include <omp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "openmp_getmax.c"

int main(int argc, char *argv[]) {
	int64_t max = 1e7;
	int64_t *factor_ct = malloc(sizeof(int64_t) * max);

	factor_ct[0] = 0;
	factor_ct[1] = 1;

	uint64_t i = 0;
	uint64_t scale = 0;

	for (i = 2; i < max; ++i) {
		factor_ct[i] = 2;
	}

#pragma omp parallel for
	for (i = 2; i <= max / 2; i++)
		for (scale = 2; scale * i < max; scale++) {
#pragma omp atomic update
			factor_ct[scale * i]++;
		}

	//	int64_t max_factors = get_max(factor_ct, max);
	//	int64_t tally[max_factors + 1];
	//	memset(tally, 0, sizeof(int64_t) * (max_factors + 1));

	// #pragma omp parallel for
	// 	for (i = 0; i < max; ++i) {
	// #pragma omp atomic update
	// 		tally[factor_ct[i]]++;
	// 	}
	//
	// 	for (i = 0; i < max_factors; ++i) {
	// 		printf("%lli\t%lli\n", i, tally[i]);
	// 	}

	free(factor_ct);

	return 0;
}
