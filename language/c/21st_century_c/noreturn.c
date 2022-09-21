/*
 * noreturn.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void wail() { fprintf(stderr, "OOOOO\n"); }

void on_death() {
	for (int i = 0; i < 3; ++i) {
		fprintf(stderr, "The program is dead...\n");
	}
}

_Noreturn void the_count() {
	for (int i = 3; i > 0; i--) {
		printf("%i\n", i);
		sleep(1);
	}
	exit(1);
}

int main() {
	// atexit works like a stack, last registered function is executed firstly.
	atexit(wail);
	atexit(on_death);
	the_count();
}
