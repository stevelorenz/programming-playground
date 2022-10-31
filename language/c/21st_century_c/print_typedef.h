/*
 * print_typedef.h
 */

#ifndef PRINT_TYPEDEF_H
#define PRINT_TYPEDEF_H

typedef struct textlist_s {
	char *title;
	char **items;
	int len;

	void (*print)(struct textlist_s *in);
} textlist_s;

#endif /* !PRINT_TYPEDEF_H */
