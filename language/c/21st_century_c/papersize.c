/*
 * papersize.c
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

typedef struct {
	double width;
	double height;
} size_s;

size_s width_height(char const* papertype) {
	if (!strcasecmp(papertype, "A4")) {
		return (size_s){.width = 210, .height = 287};
	} else if (!strcasecmp(papertype, "Letter")) {
		return (size_s){.width = 216, .height = 279};
	} else if (!strcasecmp(papertype, "Legal")) {
		return (size_s){.width = 216, .height = 356};
	}

	return (size_s){.width = NAN, .height = NAN};
}

int main(int argc, char* argv[]) {
	size_s a4size = width_height("A4");
	printf("A4 size: %g, %g\n", a4size.width, a4size.height);
	return EXIT_SUCCESS;
}
