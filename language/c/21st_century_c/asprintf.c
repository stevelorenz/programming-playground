/*
 * asprintf.c
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>	 // va_list, va_start blabla

int my_asprintf(char **str, char *fmt, ...);

int my_asprintf(char **str, char *fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	char one_char[1];
	int len = vsnprintf(one_char, 1, fmt, ap);
	if (len < 1) {
		va_end(ap);
		fprintf(stderr,
				"Can not encode the input string with the given format!\n");
		*str = NULL;
		return len;
	}
	va_end(ap);

	*str = malloc((len + 1) * sizeof(char));
	if (*str == NULL) {
		fprintf(stderr, "Can not allocate memory %d bytes for the string!",
				len + 1);
		return -1;
	}
	va_start(ap, fmt);
	vsnprintf(*str, len + 1, fmt, ap);
	va_end(ap);

	return len;
}

int main(int argc, char *argv[]) {
	char *s;
	int len = my_asprintf(&s, "hello, %s.", "Zuo");
	printf("%d\n", len);
	printf("%s\n", s);
	free(s);
	return EXIT_SUCCESS;
}
