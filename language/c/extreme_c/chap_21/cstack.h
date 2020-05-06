/*
 * cstack.h
 */

#ifndef CSTACK_H
#define CSTACK_H

#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif

#define TRUE 1
#define FALSE 0

typedef int bool_t;

struct value {
	char *data;
	size_t len;
};

struct cstack;

struct value make_value(char *data, size_t len);
struct value copy_value(char *data, size_t len);
void free_value(struct value *v);

struct cstack *cstack_new();
void cstack_delete(struct cstack *s);

void cstack_constructor(struct cstack *s, size_t max_size);
void cstack_destructor(struct cstack *s, void (*deleter)(struct value *v));

size_t cstack_size(struct cstack *s);

bool_t cstack_push(struct cstack *s, struct value v);
bool_t cstack_pop(struct cstack *s, struct value *v);

void cstack_clear(struct cstack *s, void (*deleter)(struct value *v));

#ifdef __cplusplus
}
#endif

#endif /* !CSTACK_H */
