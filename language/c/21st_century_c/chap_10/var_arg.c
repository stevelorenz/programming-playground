#include <stdio.h>
#include <math.h>

double sum_array(double in[])
{
	double out = 0;
	for (int i = 0; !isnan(in[i]); i++) {
		out += in[i];
	}
	return out;
}

// Use variable args to make sure the array ends with NAN.
#define sum(...) sum_array((double[]){ __VA_ARGS__, NAN })

int main(int argc, char *argv[])
{
	printf("Sum: %g\n", sum(1.1, 2.2, 3.3));
	return 0;
}
