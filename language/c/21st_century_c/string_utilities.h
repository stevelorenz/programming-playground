/*
 * string_utilities.h
 */

#ifndef STRING_UTILITIES_H
#define STRING_UTILITIES_H

#include <string.h>
#define _GNU_SOURCE
#include <stdio.h>

// Safer asprintf with auto-free
#define Sasprintf(write_to, ...)                \
	{                                           \
		char *tmp_string_for_extend = write_to; \
		asprintf(&(write_to), __VA_ARGS__);     \
		free(tmp_string_for_extend);            \
	}

char *string_from_file(char const *filename);

typedef struct ok_array {
	char **elements;
	char *base_string;
	int length;
} ok_array;

ok_array *ok_array_new(char *instring, char const *delimiters);

void ok_array_free(ok_array *ok_in);

#endif /* !STRING_UTILITIES_H */
