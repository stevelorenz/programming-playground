#include <stdint.h>
#include <stdio.h>

#include "openmp_getmax.c"

int main(int argc, char *argv[]) {
	int array[3] = {0, 0, 0};
	int ret = get_max(array, 10);
	printf("%i\n", ret);
	return 0;
}
