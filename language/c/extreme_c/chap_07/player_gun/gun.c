#include <stdlib.h>
#include <stdbool.h>

struct gun_t {
	int bullets;
};

struct gun_t *gun_new()
{
	return (struct gun_t *)malloc(sizeof(struct gun_t));
}

void gun_init(struct gun_t *gun, int initial_bullets)
{
	gun->bullets = 0;
	if (initial_bullets > 0) {
		gun->bullets = initial_bullets;
	}
}
void gun_destory(struct gun_t *gun)
{
}

bool gun_has_bullets(struct gun_t *gun)
{
	return (gun->bullets > 0);
}

int gun_get_bullets_num(struct gun_t *gum)
{
	return gum->bullets;
}

void gun_trigger(struct gun_t *gun)
{
	gun->bullets--;
}

void gun_refill(struct gun_t *gun)
{
	gun->bullets = 7;
}
