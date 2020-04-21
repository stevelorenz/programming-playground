/*
 * cat.h
 */

#ifndef CAT_H
#define CAT_H

struct cat_t;

struct cat_t *cat_new();
void cat_init(struct cat_t *cat);
void cat_destroy(struct cat_t *cat);

#endif /* !CAT_H */
