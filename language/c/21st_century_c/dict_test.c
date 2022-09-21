/*
 * dict_test.c
 */

#include <glib-2.0/glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

typedef struct keyval {
	char* key;
	// Again. It makes feel that C is not type safe ... :)
	void* value;
} keyval;

keyval* keyval_new(char* key, void* value) {
	keyval* out = malloc(sizeof(keyval));
	*out = (struct keyval){.key = key, .value = value};
	return out;
}

keyval* keyval_copy(struct keyval* in) {
	keyval* out = malloc(sizeof(keyval));
	*out = *in;
	return out;
}

void keyval_free(keyval* in) { free(in); }

int keyval_matches(const keyval* in, const char* key) {
	return !strcasecmp(in->key, key);
}

void* dictionary_not_found;

typedef struct dictionary {
	// In "old" C, you need to always remember that the point to the first
	// element of the array or the name of the array can be used to describe an
	// array of elements...
	struct keyval**
		pairs;	// Pairs is an array of key value pairs, so two-level pointer
	int length;
} dictionary;

dictionary* dictionary_new(void) {
	dictionary* out = malloc(sizeof(dictionary));
	*out = (dictionary){.pairs = NULL,
						.length = 0};  // Init as an empty dictionary
	return out;
}

static void dictionary_add_keyval(dictionary* in, keyval* kv) {
	in->length++;
	// Move to the next position
	in->pairs = realloc(in->pairs, in->length * sizeof(keyval*));
	in->pairs[in->length - 1] = kv;
}

void dictionary_add(dictionary* in, char* key, void* value) {
	if (!key) {
		fprintf(stderr, "NULL is not a valid key.\n");
		abort();
	}
	dictionary_add_keyval(in, keyval_new(key, value));
}

void* dictionary_find(dictionary const* in, char const* key) {
	for (int i = 0; i < in->length; i++)
		if (keyval_matches(in->pairs[i], key)) return in->pairs[i]->value;
	return dictionary_not_found;
}

dictionary* dictionary_copy(dictionary* in) {
	dictionary* out = dictionary_new();
	for (int i = 0; i < in->length; i++)
		dictionary_add_keyval(out, keyval_copy(in->pairs[i]));
	return out;
}

void dictionary_free(dictionary* in) {
	// Free EACH key-value pairs
	for (int i = 0; i < in->length; ++i) {
		keyval_free(in->pairs[i]);
	}
	// Free the pointer to the array of the keyval pair
	free(in->pairs);
	free(in);
}

typedef struct {
	dictionary* dd;
} dfixture;

void dict_setup(dfixture* df, gconstpointer test_data) {
	printf("%p\n", test_data);
	df->dd = dictionary_new();
	dictionary_add(df->dd, "key1", "val1");
	dictionary_add(df->dd, "key2", NULL);
}

void dict_teardown(dfixture* df, gconstpointer test_data) {
	dictionary_free(df->dd);
}

void check_keys(dictionary const* d) {
	char* got_it = dictionary_find(d, "xx");
	g_assert(got_it == dictionary_not_found);
	got_it = dictionary_find(d, "key1");
	g_assert_cmpstr(got_it, ==, "val1");
	got_it = dictionary_find(d, "key2");
	g_assert_cmpstr(got_it, ==, NULL);
}

void test_new(dfixture* df, gconstpointer ignored) { check_keys(df->dd); }

void test_copy(dfixture* df, gconstpointer ignored) {
	dictionary* cp = dictionary_copy(df->dd);
	check_keys(cp);
	dictionary_free(cp);
}

int main(int argc, char* argv[]) {
	g_test_init(&argc, &argv, NULL);
	// Show a minimal example of using the test harness provided by GLib
	g_test_add("/set1/new test", dfixture, NULL, dict_setup, test_new,
			   dict_teardown);
	return g_test_run();
}
