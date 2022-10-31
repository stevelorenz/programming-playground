#include "dict.h"

#include <stdio.h>
#include <stdlib.h>

void *dictionary_not_found;

dictionary *dictionary_new(void) {
	// Allocate a raw memory space withou any format
	static int dnf;
	if (!dictionary_not_found) {
		dictionary_not_found = &dnf;
	}
	dictionary *out = malloc(sizeof(dictionary));
	*out = (dictionary){};
	return out;
}

// Private function limited inside the implementation
// This is a over-simplified implementation!!! The pairs realloc every time when
// the size of the dictionary increases... In practial implementation, it should
// e.g. double its size with a pre-allocation with a capacity field.
static void dictionary_add_keyval(dictionary *in, keyval *kv) {
	in->length++;
	in->pairs = realloc(in->pairs, in->length * sizeof(keyval *));
	in->pairs[in->length - 1] = kv;
}

void dictionary_add(dictionary *in, char *key, void *value) {
	if (!key) {
		fprintf(stderr, "NULL is not a valid key!\n");
		abort();
	}
	dictionary_add_keyval(in, keyval_new(key, value));
}

dictionary *dictionary_copy(dictionary *in) {
	dictionary *out = malloc(sizeof(dictionary));
	int i = 0;
	for (i = 0; i < in->length; ++i) {
		dictionary_add_keyval(out, keyval_copy(in->pairs[i]));
	}
	return out;
}

void dictionary_free(dictionary *in) {
	// Free keyval
	for (int i = 0; i < in->length; i++) {
		keyval_free(in->pairs[i]);
	}

	// Free dictionary
	free(in->pairs);
	free(in);
}

void *dictionary_find(dictionary const *in, char const *key) {
	int i = 0;
	for (i = 0; i < in->length; ++i) {
		if (keyval_matches(in->pairs[i], key)) {
			return in->pairs[i]->value;
		}
	}
	return dictionary_not_found;
}
