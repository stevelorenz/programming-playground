/*
 * stddev.c
 */

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

struct meanvar {
	double mean;
	double var;
};

/**
 * @brief
 *
 * @param data: The pointer that points at the first element of an array of
 * doubles. This array MUST be ended with NAN
 * @return
 */
struct meanvar mean_and_var(const double *data) {
	long double avg = 0, avg2 = 0;
	long double ratio;
	size_t count = 0;
	for (size_t i = 0; !isnan(data[i]); i++) {
		ratio = (long double)count / (long double)(count + 1.0);
		count++;
		avg *= ratio;
		avg2 *= ratio;
		avg += data[i] / (count + 0.0);
		avg2 += pow(data[i], 2) / (count + 0.0);
	}
	return (struct meanvar){.mean = avg,
							.var = avg2 - pow(avg, 2)};	 // E[x^2] - E^2[x]
}

int main() {
	// NAN is defined in math.h as a Not-A-Number
	double d[] = {34124.75, 34124.48, 34124.90, 34125.31,
				  34125.05, 34124.98, NAN};
	struct meanvar mv = mean_and_var(d);
	printf("mean: %.10g var: %.10g\n", mv.mean, mv.var * 6 / 5.);

	double d2[] = {4.75, 4.48, 4.90, 5.31, 5.05, 4.98, NAN};

	mv = mean_and_var(d2);
	mv.var *= 6. / 5;
	printf("mean: %.10g var: %.10g\n", mv.mean, mv.var);

	return EXIT_SUCCESS;
}
