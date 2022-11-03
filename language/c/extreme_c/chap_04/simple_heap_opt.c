#include <stdio.h>
#include <stdlib.h>

void fill(char *ptr) {
	ptr[0] = 'w';
	ptr[1] = 'o';
	ptr[2] = 'w';
	ptr[3] = '!';
	ptr[4] = '\0';
	ptr[5] = 'a';
}

int main(int argc, char *argv[]) {
	void *gptr = malloc(1024);
	char *ptr = (char *)(gptr);
	fill(ptr);
	printf("%s\n", ptr);
	free(ptr);

	return 0;
}
