/*
 * hash_table.c
 */

#include <stdlib.h>
#include <string.h>

#define NHASH 17

enum { MULTIPLIER = 31 };

// Compute hash value of a string
unsigned int hash(const char *str)
{
	unsigned int h = 0;
	unsigned char *p = NULL;
	for (p = (unsigned char *)str; *p != '\0'; p++) {
		h = MULTIPLIER * h + *p;
	}
	h = h % NHASH;
	return h;
}

typedef struct Nameval Nameval_t;
struct Nameval {
	char *name;
	int value;
	Nameval_t *next;
};

Nameval_t *symtab[NHASH];

Nameval_t *lookup(char *name, int create, int value)
{
	int h;
	Nameval_t *sym = NULL;
	h = hash(name);
	// Search over the hash chain.
	for (sym = symtab[h]; sym != NULL; sym = sym->next) {
		if (strcmp(name, sym->name) == 0) {
			return sym;
		}
	}
	if (create) {
		sym = (Nameval_t *)malloc(sizeof(Nameval_t));
		sym->name = name; // assumed allocated somewhere else.
		sym->value = value;
		sym->next = symtab[h];
		symtab[h] = sym;
	}

	return sym;
}

int main()
{
	return 0;
}
