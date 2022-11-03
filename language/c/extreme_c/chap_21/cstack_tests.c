#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "cstack.h"

void deleter(struct value *v) {
	if (v->data) {
		free(v->data);
	}
	v->data = NULL;
}

struct value make_int(int int_value) {
	struct value v;
	int *int_ptr;
	int_ptr = malloc(sizeof(int));
	*int_ptr = int_value;
	v.data = (char *)int_ptr;
	v.len = sizeof(int);
	return v;
}

int extract_int(struct value *v) { return *((int *)v->data); }

int main(int argc, char *argv[]) {
	struct cstack *s = cstack_new();

	cstack_constructor(s, 100);
	assert(cstack_size(s) == 0);

	int int_values[] = {5, 10, 20, 30};
	size_t i;
	for (i = 0; i < 4; ++i) {
		cstack_push(s, make_int(int_values[i]));
	}
	assert(cstack_size(s) == 4);

	int counter = 3;
	struct value v;
	while (cstack_size(s) > 0) {
		bool_t popped = cstack_pop(s, &v);
		assert(popped);
		assert(extract_int(&v) == int_values[counter--]);
		// Free the memory allocated by the make_int function.
		deleter(&v);
	}
	assert(counter == -1);

	cstack_clear(s, deleter);
	assert(cstack_size(s) == 0);

	cstack_destructor(s, deleter);
	cstack_delete(s);
	printf("All tests were OK.\n");
	return 0;
}
