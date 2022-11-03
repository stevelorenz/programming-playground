#include "player.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gun.h"

struct player_t {
	char *name;
	struct gun_t *gun;
};

struct player_t *player_new() {
	return (struct player_t *)malloc(sizeof(struct player_t));
}

void player_init(struct player_t *player, const char *name) {
	player->name = malloc((strlen(name) + 1) * sizeof(char));
	strncpy(player->name, name, strlen(name));
	/* In aggregation, the pointer should be nullified! */
	player->gun = NULL;
}

void player_destroy(struct player_t *player) { free(player->name); }

void player_pickup_gun(struct player_t *player, struct gun_t *gun) {
	player->gun = gun;
}

void player_shoot(struct player_t *player) {
	if (player->gun) {
		gun_trigger(player->gun);
		printf("Player shooted, number of bullets rest: %d\n",
			   gun_get_bullets_num(player->gun));
	} else {
		printf("The player wants to shoot but he does not have a gun!\n");
		exit(1);
	}
}

void player_drop_gun(struct player_t *player) { player->gun = NULL; }
