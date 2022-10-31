#include "keyval.h"

#include <stdlib.h>
#include <strings.h>

keyval *keyval_new(char *key, void *value) {
	keyval *out = malloc(sizeof(keyval));
	// Struct compounded structure
	*out = (keyval){.key = key, .value = value};
	return out;
}

keyval *keyval_copy(keyval const *in) {
	keyval *out = malloc(sizeof(keyval));
	*out = *in;
	return out;
}

void keyval_free(keyval *in) { free(in); }

int keyval_matches(keyval const *in, char const *key) {
	return !strcasecmp(in->key, key);
}
