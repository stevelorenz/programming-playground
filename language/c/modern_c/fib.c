#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

size_t fib(size_t n)
{
	if (n < 3)
		return 1;
	else
		return fib(n - 1) + fib(n - 2);
}

/**
 * @ Compute fab with the help of a cache that may hold previously computed
 * values. So space and time tradeoff.
 */
size_t fib_cached_rec(size_t n, size_t cache[n])
{
	// Use n-1 because it is a index of the array.
	if (!cache[n - 1]) {
		cache[n - 1] = fib_cached_rec(n - 1, cache) +
			       fib_cached_rec(n - 2, cache);
	}
	return cache[n - 1];
}

size_t fib_cached(size_t n)
{
	if (n < 3) {
		return 1;
	}
	size_t cache[n];
	cache[0] = 1;
	cache[1] = 1;
	size_t i;
	for (i = 2; i < n; ++i) {
		cache[i] = 0;
	}
	return fib_cached_rec(n, cache);
}

int main(int argc, char *argv[])
{
	struct timeval start = { 0 };
	struct timeval current = { 0 };
	uint64_t duration_ms = 0;
	size_t n = 40;
	printf("n = %lu\n", n);

	size_t result = 0;
	// WARN: gettimeofday is not that accurate...
	gettimeofday(&start, NULL);
	result = fib(n);
	gettimeofday(&current, NULL);
	duration_ms = 1000 * (current.tv_sec - start.tv_sec) +
		      (current.tv_usec - start.tv_usec) / 1000;
	printf("The duration of fib: %lu, result:%lu\n", duration_ms, result);

	gettimeofday(&start, NULL);
	result = fib_cached(n);
	gettimeofday(&current, NULL);
	duration_ms = 1000 * (current.tv_sec - start.tv_sec) +
		      (current.tv_usec - start.tv_usec) / 1000;
	printf("The duration of fib cached: %lu, result:%lu\n", duration_ms,
	       result);

	return EXIT_SUCCESS;
}
