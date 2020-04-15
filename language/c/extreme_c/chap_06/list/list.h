/*
 * dummy_list.h
 *
 * An example to show how to have private attributes and behaviors (methods) in
 * pure C.
 */

#ifndef LIST_H
#define LIST_H

#include <unistd.h>

struct list_t;

// Allocation function
struct list_t *list_malloc();

// Constructor and destructor
void list_init(struct list_t *list);
void list_destroy(struct list_t *list);

// Public behavior functions

/**
 * @brief Add an item into the list.
 *
 * @return
 *	- 0 on sucess.
 *	-1 if the list is full.
 */
int list_add(struct list_t *list, int item);
int list_get(struct list_t *list, int index, int *result);
void list_clear(struct list_t *list);
size_t list_size(struct list_t *list);
void list_print(struct list_t *list);

#endif /* !LIST_H */
