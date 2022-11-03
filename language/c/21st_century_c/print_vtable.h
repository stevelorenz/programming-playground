/*
 * print_vtable.h
 */

#ifndef PRINT_VTABLE_H
#define PRINT_VTABLE_H

#include <glib.h>

#include "print_typedef.h"

// Use extern to share global variable among multiple files
extern GHashTable *print_fns;

typedef void (*print_fn_type)(textlist_s *in);

void check_print_fn(print_fn_type pf);

// Hash the address of the function to a function -> vtable
#define print_hash_add(object, print_fn)                           \
	{                                                              \
		check_print_fn(print_fn);                                  \
		g_hash_table_insert(print_fns, (object)->print, print_fn); \
	}

void textlist_print_html(textlist_s *in);

#endif /* !PRINT_VTABLE_H */
