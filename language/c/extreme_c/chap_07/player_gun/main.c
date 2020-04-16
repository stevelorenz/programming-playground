#include <stdio.h>
#include <stdlib.h>

#include "player.h"
#include "gun.h"

int main(int argc, char *argv[])
{
	struct player_t *player;
	struct gun_t *gun;

	player = player_new();
	gun = gun_new();

	player_init(player, "Player 1");
	gun_init(gun, 3);

	player_pickup_gun(player, gun);

	while (gun_has_bullets(gun)) {
		player_shoot(player);
	}
	gun_refill(gun);
	while (gun_has_bullets(gun)) {
		player_shoot(player);
	}
	player_drop_gun(player);

	gun_destory(gun);
	player_destroy(player);
	free(player);
	free(gun);
	return 0;
}
