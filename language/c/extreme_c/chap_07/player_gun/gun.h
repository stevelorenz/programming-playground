/*
 * gun.h
 */

#ifndef GUN_H
#define GUN_H

#include <stdbool.h>

struct gun_t;

struct gun_t *gun_new();
void gun_init(struct gun_t *gun, int initial_bullets);
void gun_destory(struct gun_t *gun);

bool gun_has_bullets(struct gun_t *gun);
int gun_get_bullets_num(struct gun_t *gum);
void gun_trigger(struct gun_t *gun);
void gun_refill(struct gun_t *gun);

#endif /* !GUN_H */
