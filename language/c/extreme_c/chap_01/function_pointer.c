/*
 * Simple function pointers.
 * */

#include <stdio.h>

/* Define a template for function pointers with arguments. */
typedef int (*operation_t)(int, int);

int sum(int a, int b) { return a + b; }

int subtract(int a, int b) { return a - b; }

int main(int argc, char *argv[]) {
	operation_t func_ptr = NULL;

	func_ptr = sum;
	printf("Sum result: %d\n", func_ptr(1, 2));
	func_ptr = subtract;
	printf("Subtract result: %d\n", func_ptr(1, 2));

	return 0;
}
