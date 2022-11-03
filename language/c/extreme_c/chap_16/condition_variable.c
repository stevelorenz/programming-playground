#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

struct shared_state_t {
	// A flag which indicates wether 'A' has been printed out or not.
	int done;
	// The mutex to protect the critical section.
	pthread_mutex_t mtx;
	// The conditional variable.
	pthread_cond_t cv;
};

void shared_state_init(struct shared_state_t *state) {
	state->done = FALSE;
	pthread_mutex_init(&state->mtx, NULL);
	pthread_cond_init(&state->cv, NULL);
}

void shared_state_destroy(struct shared_state_t *state) {
	pthread_mutex_destroy(&state->mtx);
	pthread_cond_destroy(&state->cv);
}

void *thread_body_1(void *arg) {
	struct shared_state_t *ss = (struct shared_state_t *)arg;
	pthread_mutex_lock(&ss->mtx);
	printf("A\n");
	ss->done = TRUE;
	// Signals the threads waiting on the conditional variable
	pthread_cond_signal(&ss->cv);
	pthread_mutex_unlock(&ss->mtx);
	return NULL;
}

void *thread_body_2(void *arg) {
	struct shared_state_t *ss = (struct shared_state_t *)arg;
	pthread_mutex_lock(&ss->mtx);
	// Wait until the flag become True.
	// Thread 2 can be notified by other resources rather than Thread 1.
	// So here a while loop is used instead of if.
	while (!ss->done) {
		pthread_cond_wait(&ss->cv, &ss->mtx);
	}
	printf("B\n");
	pthread_mutex_unlock(&ss->mtx);
	return NULL;
}

int main(int argc, char *argv[]) {
	struct shared_state_t state;
	pthread_t thread1;
	pthread_t thread2;
	int result1;
	int result2;

	shared_state_init(&state);
	result1 = pthread_create(&thread1, NULL, thread_body_1, &state);
	result2 = pthread_create(&thread2, NULL, thread_body_2, &state);

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

	shared_state_destroy(&state);

	return 0;
}
