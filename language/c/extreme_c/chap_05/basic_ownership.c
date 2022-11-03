#include <stdio.h>
#include <stdlib.h>

#define QUEUE_MAX_SIZE 100

struct queue {
	int front;
	int rear;
	double *arr;
};

void queue_init(struct queue *q) {
	q->front = 0;
	q->rear = 0;
	/* The queue object is the owner of this heap memory.*/
	q->arr = malloc(QUEUE_MAX_SIZE * sizeof(double));
}

void queue_destroy(struct queue *q) { free(q->arr); }

int queue_size(struct queue *q) { return q->rear - q->front; }

void enqueue(struct queue *q, double item) {
	q->arr[q->rear] = item;
	q->rear++;
}

double dequeue(struct queue *q) {
	double item = q->arr[q->front];
	q->front++;
	return item;
}

int main(int argc, char *argv[]) {
	struct queue *q;
	q = malloc(sizeof(struct queue));
	queue_init(q);

	enqueue(q, 1.0);
	enqueue(q, 2.0);
	printf("The current queue size: %d\n", queue_size(q));
	printf("Dequeue: %f\n", dequeue(q));
	printf("Dequeue: %f\n", dequeue(q));

	queue_destroy(q);
	free(q);
	return 0;
}
