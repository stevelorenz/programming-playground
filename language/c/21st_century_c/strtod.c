/*
 * strtod.c
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "stopif.h"

double average(int count, ...) {
	va_list ap;
	double sum = 0;

	va_start(ap, count);
	for (int i = 0; i < count; ++i) {
		sum += va_arg(ap, int);
	}
	va_end(ap);
	return sum / count;
}

int main(int argc, char* argv[]) {
	double ret = 0;
	ret = average(3, 1, 2, 3);
	printf("(Just test variadic function) The average is %g\n", ret);

	char error_mode = 's';
	PRINT_ERROR_MODE;
	error_mode = 'a';
	PRINT_ERROR_MODE;

	error_mode = 's';

	STOP_IF(argc < 2, return EXIT_FAILURE,
			"Please input a number in CLI to be squared!");

	printf("Input number: %s\n", argv[1]);

	char* end;
	double in = strtod(argv[1], &end);
	STOP_IF(*end, return EXIT_FAILURE,
			"Can not parse %s into double! Problem with %s\n", argv[1], end);

	ret = in * in;
	printf("The square of %s is %g\n", argv[1], ret);

	return EXIT_SUCCESS;
}
