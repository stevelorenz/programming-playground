#include <glib-2.0/glib.h>

void hash_a_char(gunichar uc, GHashTable *hash);
void printone(void *key_in, void *val_in, void *xx);
GHashTable *new_unicode_couting_hash();
