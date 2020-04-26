#include <stdio.h>
#include <stdlib.h>

enum taste { SWEET, SOUR };

char *taste_string[] = { "SWEET", "SOUR" };

struct eatable_t {
	// Q: Why use void* pointer here instead of struct etable_class* ?
	enum taste (*get_taste_func)(struct eatable_t *self);
};

void eatable_init(struct eatable_t *eatable)
{
	// We do not have a default implementation for the virtual function.
	eatable->get_taste_func = NULL;
}

enum taste eatable_get_taste(struct eatable_t *eatable)
{
	return eatable->get_taste_func(eatable);
}

struct apple_t {
	struct eatable_t base;
};

enum taste __apple_get_taste_func(struct eatable_t *self)
{
	return SWEET;
}

struct apple_t *apple_new()
{
	return malloc(sizeof(struct apple_t));
}

void apple_init(struct apple_t *apple)
{
	apple->base.get_taste_func = __apple_get_taste_func;
}

void apple_destroy(struct apple_t *a)
{
}

int main(int argc, char *argv[])
{
	struct apple_t *a;
	a = apple_new();
	apple_init(a);
	enum taste t;
	t = eatable_get_taste((struct eatable_t *)a);
	printf("The taste of the eatable is %s\n", taste_string[t]);

	apple_destroy(a);
	free(a);
	return 0;
}
