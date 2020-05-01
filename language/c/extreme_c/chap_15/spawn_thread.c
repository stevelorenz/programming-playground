#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

void *thread_body(void *args)
{
	printf("Hello World from the first thread.\n");
	return NULL;
}

int main(int argc, char *argv[])
{
	pthread_t thread;
	int result;
	result = pthread_create(&thread, NULL, thread_body, NULL);
	if (result != 0) {
		printf("Thread can not be created, error number: %d\n", result);
		exit(1);
	}

	// Wait for the created thread to finish its job.
	result = pthread_join(thread, NULL);
	if (result != 0) {
		printf("The thread can not be joined!\n");
		exit(2);
	}
	printf("Hello from the main thread!\n");

	return 0;
}
