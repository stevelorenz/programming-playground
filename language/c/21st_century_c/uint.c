/*
 * uint.c
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	int neg = -2;
	size_t zero = 0;

	// MARK: When compare unsigned integer and signed integer,
	// C will surprisingly convert signed integer to unsigned integer... That
	// can lead to issue
	// if (neg < zero) {
	// 	printf("Yes! -2 is less than zero!\n");
	// } else {
	// 	printf("No! -2 is bigger than zero!\n");
	// }

	if (neg < 0) {
		printf("Yes! -2 is less than zero!\n");
	} else {
		printf("No! -2 is bigger than zero!\n");
	}

	return EXIT_SUCCESS;
}
