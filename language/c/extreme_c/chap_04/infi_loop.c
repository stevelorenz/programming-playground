#include <stdlib.h>
#include <unistd.h>

int main(int argc, char *argv[]) {
	void *p = malloc(1024);	 // allocate 1KB from heap.
	while (1) {
		sleep(1);
	}
	return 0;
}
