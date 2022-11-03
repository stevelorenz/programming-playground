#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "list.h"

/**
 * @brief Reverse all elements in the source list into the destination list.
 *
 * @return
 *	- 0 on success.
 *	- -1 if can not get any element in the source list.
 */
int list_reverse(struct list_t *source, struct list_t *dest) {
	int i = 0;
	int item = 0;
	list_clear(dest);
	if (list_size(source) == 0) {
		return -1;
	}
	for (i = list_size(source) - 1; i >= 0; i--) {
		if (list_get(source, i, &item) == -1) {
			return -1;
		}
		list_add(dest, item);
	}
	return 0;
}

int main(int argc, char *argv[]) {
	struct list_t *list1;
	struct list_t *list2;

	list1 = list_malloc();
	list2 = list_malloc();
	list_init(list1);
	list_init(list2);
	assert(list_reverse(list1, list2) == -1);

	list_add(list1, 1);
	list_add(list1, 2);
	list_add(list1, 3);
	list_add(list1, 4);

	printf("Print the list 1:\n");
	list_print(list1);
	int result = 0;
	if (list_get(list1, 1, &result) == 0) {
		printf("The second element in the list1 is %d.\n", result);
	}
	assert(list_get(list1, -1, &result) == -1);
	assert(list_get(list1, 4, &result) == -1);

	list_reverse(list1, list2);
	printf("Print the list 2 (reversed list1):\n");
	list_print(list2);

	list_destroy(list1);
	list_destroy(list2);
	free(list1);
	free(list2);

	return 0;
}
