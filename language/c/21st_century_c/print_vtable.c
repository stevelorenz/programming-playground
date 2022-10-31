#include "print_vtable.h"

#include <stdio.h>

GHashTable *print_fns;

void check_print_fn(print_fn_type pf) {
	printf("Checking should be performed here!\n");
}

void textlist_print_html(textlist_s *in) {
	if (!print_fns) {
		print_fns = g_hash_table_new(g_direct_hash, g_direct_equal);
	}

	print_fn_type ph = g_hash_table_lookup(print_fns, in->print);
	if (ph) {
		ph(in);
		return;
	}

	// Run the default behavior
	printf("<title>%s</title>\n<ul>", in->title);
	for (int i = 0; i < in->len; i++) {
		printf("<li>%s</li>\n", in->items[i]);
	};
	printf("</ul>\n");
}
