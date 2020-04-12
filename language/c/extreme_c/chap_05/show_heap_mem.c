#include <stdio.h>
#include <stdlib.h>

void print_mem_maps()
{
#ifdef __linux__
	FILE *fd = fopen("/proc/self/maps", "r");
	if (fd == NULL) {
		printf("Can not open maps file!\n");
		exit(1);
	}
	char line[1024];
	while (!feof(fd)) {
		fgets(line, 1024, fd);
		printf("> %s\n", line);
	}

	fclose(fd);
#endif
}

int main(int argc, char *argv[])
{
	char *p;
	p = malloc(10 * sizeof(char));
	printf("Malloc: ");
	for (int i = 0; i < 10; i++) {
		printf("0x%02x ", (unsigned char)p[i]);
	}
	printf("\n");

	char *q;
	q = calloc(10, sizeof(char));
	printf("Calloc: ");
	for (int i = 0; i < 10; i++) {
		printf("0x%02x ", (unsigned char)q[i]);
	}

	printf("\n");
	printf("Current memory map: \n");
	print_mem_maps();

	free(p);
	free(q);
	return 0;
}
