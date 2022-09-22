/*
 * constchange.c
 */

#include <stdlib.h>

// Although b is a const pointer which does not allow modification.
// But a and b are pointing to the same value, so a can be used to modifidy the
// value that is pointed by b.
void set_element(int* a, int const* b) { a[0] = 3; }

int main(int argc, char* argv[]) {
	int a[10] = {};
	int const* b = a;
	set_element(a, b);
	return EXIT_SUCCESS;
}
