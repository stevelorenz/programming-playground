/*
 * player.h
 */

#ifndef PLAYER_H
#define PLAYER_H

struct player_t;
struct gun_t;

struct player_t *player_new();
void player_init(struct player_t *player, const char *name);
void player_destroy(struct player_t *player);

void player_pickup_gun(struct player_t *player, struct gun_t *gun);
void player_shoot(struct player_t *player);
void player_drop_gun(struct player_t *player);

#endif /* !PLAYER_H */
