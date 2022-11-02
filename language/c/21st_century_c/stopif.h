#include <stdio.h>
#include <stdlib.h>

// Set this to 's' to stop the program on an error (abort).
// Otherwise, functions take error_action on failure.
char error_mode;

#define PRINT_ERROR_MODE printf("Current error mode: %c\n", error_mode)

// __VA_ARGS__: Variadic macro! Expand ... to arbitary number of arguments.
#define STOP_IF(assertion, error_action, ...) \
	{                                         \
		if (assertion) {                      \
			fprintf(stderr, __VA_ARGS__);     \
			fprintf(stderr, "\n");            \
			if (error_mode == 's')            \
				abort();                      \
			else {                            \
				error_action;                 \
			}                                 \
		}                                     \
	}
