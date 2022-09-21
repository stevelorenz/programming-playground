#include <stdint.h>
#include <stdio.h>

long long int fibonacci() {
	// Static local variable only initializes once and exists until the end of
	// the program. So it can be used to implement state machine!
	static _Thread_local long long int first = 0;
	static _Thread_local long long int second = 1;
	long long int out = first + second;
	first = second;
	second = out;
	return out;
}

int main(void) {
	for (int i = 0; i < 50; i++) {
		printf("%lli\n", fibonacci());
	}
}
