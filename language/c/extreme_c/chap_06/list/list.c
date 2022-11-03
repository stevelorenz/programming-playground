/*
 * list.c
 */

#include "list.h"

#include <stdio.h>
#include <stdlib.h>

#define LIST_MAX_SIZE 10

struct list_t {
	size_t size;
	int *items;
};

/* A private behavior to check if the list is full. */
int __list_is_full(struct list_t *list) {
	return (list->size == LIST_MAX_SIZE);
}

/* A private behavior to check if the index is valid. */
int __list_index_is_valid(struct list_t *list, const int index) {
	return (index >= 0 && index < list->size);
}

struct list_t *list_malloc() {
	return (struct list_t *)malloc(sizeof(struct list_t));
}

void list_init(struct list_t *list) {
	list->size = 0;
	list->items = malloc(LIST_MAX_SIZE * sizeof(int));
}

void list_destroy(struct list_t *list) { free(list->items); }

int list_add(struct list_t *list, int item) {
	if (__list_is_full(list)) {
		return -1;
	}
	list->items[list->size++] = item;
	return 0;
}

int list_get(struct list_t *list, int index, int *result) {
	if (__list_index_is_valid(list, index)) {
		*result = list->items[index];
		return 0;
	}
	return -1;
}

void list_clear(struct list_t *list) { list->size = 0; }

size_t list_size(struct list_t *list) { return list->size; }

void list_print(struct list_t *list) {
	int i = 0;
	printf("[");
	for (i = 0; i < list->size; ++i) {
		printf("%d,", list->items[i]);
	}
	printf("]\n");
}
