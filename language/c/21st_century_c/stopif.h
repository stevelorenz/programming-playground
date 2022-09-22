#include <stdio.h>
#include <stdlib.h>

extern char error_mode;

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
