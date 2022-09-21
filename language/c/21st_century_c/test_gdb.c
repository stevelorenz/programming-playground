/*
 * test_gdb.c
 */

#include <glib-2.0/glib.h>
#include <stdio.h>
#include <stdlib.h>

GList *list = NULL;

int main() {
	int x[20] = {0};
	x[0] = 3;
	printf("x[0]=%d\n", x[0]);

	list = g_list_append(list, "a");
	list = g_list_append(list, "b");
	list = g_list_append(list, "c");

	for (; list != NULL; list = list->next) {
		printf("%s, ", (char *)(list->data));
	}
	printf("\n");

	return EXIT_SUCCESS;
}
