#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <stdlib.h>

// A barrier to make hydrogen and oxygen threads synchronized.
pthread_barrier_t water_barrier;
pthread_mutex_t oxygen_mutex;
// A generall semaphore to make hydrogen threads synchronized.
sem_t *hydrogen_sm;

unsigned int water_molecules_num = 0;

void *hydrogen_thread_body(void *arg) {
	// Two hydrogen threads can enter this critical section.
	sem_wait(hydrogen_sm);
	// Wait for one oxygen thread to join.
	pthread_barrier_wait(&water_barrier);
	sem_post(hydrogen_sm);
	return NULL;
}

void *oxygen_thread_body(void *arg) {
	pthread_mutex_lock(&oxygen_mutex);
	// Wait for hydrogen threads to join.
	pthread_barrier_wait(&water_barrier);
	water_molecules_num++;
	pthread_mutex_unlock(&oxygen_mutex);
	return NULL;
}

int main(int argc, char *argv[]) {
	water_molecules_num = 0;
	pthread_mutex_init(&oxygen_mutex, NULL);
	sem_t local_sem;
	hydrogen_sm = &local_sem;
	// 0->shared by threads.
	sem_init(hydrogen_sm, 0, 2);
	pthread_barrier_init(&water_barrier, NULL, 3);

	pthread_t thread[150];
	size_t i;

	for (i = 0; i < 50; ++i) {
		if (pthread_create(&thread[i], NULL, oxygen_thread_body, NULL)) {
			printf("Can not create oxygen thread.\n");
			exit(1);
		}
	}

	for (i = 50; i < 150; ++i) {
		if (pthread_create(&thread[i], NULL, hydrogen_thread_body, NULL)) {
			printf("Can not create hydrogen thread.\n");
			exit(2);
		}
	}

	for (i = 0; i < 150; ++i) {
		if (pthread_join(thread[i], NULL)) {
			printf("Can not join a thread.\n");
			exit(3);
		}
	}

	printf("The number of water molecules: %d\n", water_molecules_num);

	sem_destroy(hydrogen_sm);

	return 0;
}
