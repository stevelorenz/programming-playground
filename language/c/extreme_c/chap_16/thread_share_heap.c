#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define CHECK_RESULT(result)                   \
	if (result) {                              \
		printf("A pthread error happened.\n"); \
		exit(1);                               \
	}

int TRUE = 1;
int FALSE = 0;

char *shared_array;
size_t shared_array_len;

pthread_barrier_t alloc_barrier;
pthread_barrier_t fill_barrier;
pthread_barrier_t done_barrier;

void *alloc_thread_body(void *arg) {
	shared_array_len = 20;
	shared_array = malloc(shared_array_len * sizeof(char *));
	pthread_barrier_wait(&alloc_barrier);
	return NULL;
}

void *filler_thread_body(void *arg) {
	// Wait for the alloc thread to finish his job.
	pthread_barrier_wait(&alloc_barrier);
	int even = *((int *)arg);
	// For odd index
	char c = 'a';
	size_t start_index = 1;
	size_t i;

	if (even) {
		c = 'Z';
		start_index = 0;
	}

	for (i = start_index; i < shared_array_len; i += 2) {
		shared_array[i] = even ? c-- : c++;
	}
	shared_array[shared_array_len - 1] = '\0';
	pthread_barrier_wait(&fill_barrier);
	return NULL;
}

void *print_thread_body(void *arg) {
	pthread_barrier_wait(&fill_barrier);
	printf(">> %s\n", shared_array);
	pthread_barrier_wait(&done_barrier);
	return NULL;
}

void *destroy_thread_body(void *arg) {
	pthread_barrier_wait(&done_barrier);
	free(shared_array);
	pthread_barrier_destroy(&alloc_barrier);
	pthread_barrier_destroy(&fill_barrier);
	pthread_barrier_destroy(&done_barrier);
	return NULL;
}

int main(int argc, char *argv[]) {
	shared_array = NULL;
	pthread_barrier_init(&alloc_barrier, NULL, 3);
	pthread_barrier_init(&fill_barrier, NULL, 3);
	pthread_barrier_init(&done_barrier, NULL, 2);

	pthread_t alloc_thread;
	pthread_t odd_fill_thread;
	pthread_t even_fill_thread;
	pthread_t print_thread;
	pthread_t destroy_thread;
	int res = 0;
	pthread_attr_t attr;

	// Make all threads detached, so no join is required. The process will
	// not terminate when main thread exit.
	pthread_attr_init(&attr);
	res = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
	CHECK_RESULT(res);
	res = pthread_create(&alloc_thread, &attr, alloc_thread_body, NULL);
	CHECK_RESULT(res);
	res = pthread_create(&even_fill_thread, &attr, filler_thread_body, &TRUE);
	CHECK_RESULT(res);
	res = pthread_create(&odd_fill_thread, &attr, filler_thread_body, &FALSE);
	CHECK_RESULT(res);

	res = pthread_create(&print_thread, &attr, print_thread_body, NULL);
	CHECK_RESULT(res);

	res = pthread_create(&destroy_thread, &attr, destroy_thread_body, NULL);
	CHECK_RESULT(res);

	pthread_exit(NULL);

	return 0;
}
