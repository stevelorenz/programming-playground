/*
 * compund_literal.c
 */

#include <stdio.h>
#include <math.h>

double sum(double in[])
{
	double out = 0;
	for (int i = 0; !isnan(in[i]); i++) {
		out += in[i];
	}
	return out;
}

int main(int argc, char *argv[])
{
	double list[] = { 1.1, 2.2, 3.3, NAN };
	printf("Sum: %g\n", sum(list));
	printf("Sum: %g\n", sum((double[]){ 1.1, 2.2, 3.3, NAN }));
	return 0;
}
