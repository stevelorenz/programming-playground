#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

void *thread_body(void *args) {
	char *str = (char *)(args);
	printf("%s\n", str);
	return NULL;
}

int main(int argc, char *argv[]) {
	pthread_t thread1;
	pthread_t thread2;
	pthread_t thread3;
	int result1;
	int result2;
	int result3;

	result1 = pthread_create(&thread1, NULL, thread_body, "Apple");
	result2 = pthread_create(&thread2, NULL, thread_body, "Orange");
	result3 = pthread_create(&thread3, NULL, thread_body, "Lemon");

	if (result1 || result2 || result3) {
		printf("Threads can not be created.\n");
		exit(1);
	}

	// Wait for the created thread to finish its job.
	result1 = pthread_join(thread1, NULL);
	result2 = pthread_join(thread2, NULL);
	result3 = pthread_join(thread3, NULL);
	if (result1 || result2 || result3) {
		printf("Threads can not be joined.\n");
		exit(1);
	}

	printf("Hello from the main thread!\n");

	return 0;
}
