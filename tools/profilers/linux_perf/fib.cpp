/*
 * fib.cpp
 */

#include <cstdint>

int fib(int x) {
	if (x == 0) {
		return 0;
	} else if (x == 1) {
		return 1;
	} else
		return fib(x - 1) + fib(x - 2);
}

int main(int argc, char *argv[]) {
	for (uint32_t i = 0; i < 45; ++i) {
		fib(i);
	}
}
