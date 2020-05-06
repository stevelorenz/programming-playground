#include <stdlib.h>
#include <assert.h>

#include "cstack.h"

struct cstack {
	size_t top;
	size_t max_size;
	struct value *values;
};

struct value make_value(char *data, size_t len)
{
	struct value v;
	v.data = data;
	v.len = len;
	return v;
}

struct value copy_value(char *data, size_t len)
{
	char *buf;
	size_t i;

	buf = malloc(len * sizeof(char));
	for (i = 0; i < len; ++i) {
		buf[i] = data[i];
	}
	return make_value(buf, len);
}

void free_value(struct value *v)
{
	if (v) {
		if (v->data) {
			free(v->data);
			v->data = NULL;
		}
	}
}

struct cstack *cstack_new()
{
	return (struct cstack *)malloc(sizeof(struct cstack));
}

void cstack_delete(struct cstack *s)
{
	free(s);
}

void cstack_constructor(struct cstack *s, size_t max_size)
{
	s->top = 0;
	s->max_size = max_size;
	s->values = (struct value *)malloc(max_size * sizeof(struct value));
}

void cstack_destructor(struct cstack *s, void (*deleter)(struct value *v))
{
	cstack_clear(s, deleter);
	free(s->values);
}

size_t cstack_size(struct cstack *s)
{
	return s->top;
}

bool_t cstack_push(struct cstack *s, struct value v)
{
	if (s->top < s->max_size) {
		s->top += 1;
		s->values[s->top] = v;
		return TRUE;
	}
	return FALSE;
}

bool_t cstack_pop(struct cstack *s, struct value *v)
{
	if (s->top > 0) {
		*v = s->values[s->top];
		s->top -= 1;
		return TRUE;
	}
	return FALSE;
}

void cstack_clear(struct cstack *s, void (*deleter)(struct value *v))
{
	struct value v;
	while (cstack_size(s) > 0) {
		bool_t popped = cstack_pop(s, &v);
		assert(popped);
		if (deleter) {
			deleter(&v);
		}
	}
}
