#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

pthread_mutex_t mtx;

void *thread_body_1(void *args)
{
	int *shared_var_ptr = (int *)args;
	pthread_mutex_lock(&mtx);
	(*shared_var_ptr)++;
	printf("%d\n", *shared_var_ptr);
	pthread_mutex_unlock(&mtx);
	return NULL;
}

void *thread_body_2(void *args)
{
	int *shared_var_ptr = (int *)args;
	pthread_mutex_lock(&mtx);
	(*shared_var_ptr) += 2;
	printf("%d\n", *shared_var_ptr);
	pthread_mutex_unlock(&mtx);
	return NULL;
}

int main(int argc, char *argv[])
{
	int shared_var = 0;
	pthread_t thread1;
	pthread_t thread2;
	int result1;
	int result2;

	pthread_mutex_init(&mtx, NULL);
	result1 = pthread_create(&thread1, NULL, thread_body_1, &shared_var);
	result2 = pthread_create(&thread2, NULL, thread_body_2, &shared_var);

	if (result1 || result2) {
		printf("Threads can not be created.\n");
		exit(1);
	}

	// Wait for the created thread to finish its job.
	result1 = pthread_join(thread1, NULL);
	result2 = pthread_join(thread2, NULL);
	if (result1 || result2) {
		printf("Threads can not be joined.\n");
		exit(1);
	}
	printf("Main thread: %d\n", shared_var);

	pthread_mutex_destroy(&mtx);

	return 0;
}
