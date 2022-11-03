#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

void *thread_body_1(void *arg) {
	int local_var = 0;
	printf("Thread 1 > Stack Address: %p\n", (void *)&local_var);
	return NULL;
}

void *thread_body_2(void *arg) {
	int local_var = 0;
	printf("Thread 2 > Stack Address: %p\n", (void *)&local_var);
	return NULL;
}

int main(int argc, char *argv[]) {
	// The allocated buffer should have a minimum size to be used as the
	// stack region of a thread.
	size_t buffer_len = PTHREAD_STACK_MIN + 100;
	char *buffer;
	buffer = malloc(buffer_len * sizeof(char));

	pthread_t thread1;
	pthread_t thread2;
	int result1;
	int result2;

	pthread_attr_t attr;
	pthread_attr_init(&attr);
	if (pthread_attr_setstack(&attr, buffer, buffer_len)) {
		printf("Faild to set the thread attributes.\n");
		exit(1);
	}

	result1 = pthread_create(&thread1, NULL, thread_body_1, NULL);
	result2 = pthread_create(&thread2, &attr, thread_body_2, NULL);
	if (result1 || result2) {
		printf("Faild to create the threads.\n");
		exit(2);
	}

	result1 = pthread_join(thread1, NULL);
	result2 = pthread_join(thread2, NULL);
	if (result1 || result2) {
		printf("Faild to join the threads.\n");
		exit(2);
	}
	printf("Main Thread > Heap Address: %p\n", (void *)buffer);
	printf("Main Thread > Stack Address: %p\n", (void *)&buffer_len);

	free(buffer);
	return 0;
}
