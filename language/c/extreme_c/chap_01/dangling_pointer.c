#include <stdio.h>


/**
 * Show segmentation fault caused by dangling pointers.
 */
int *create_integer(int default_value)
{
	int var = default_value;
	return &var;
}

int main(int argc, char *argv[])
{
	int *var;
	var = create_integer(10);
	printf("%d\n", *var);
	return 0;
}
