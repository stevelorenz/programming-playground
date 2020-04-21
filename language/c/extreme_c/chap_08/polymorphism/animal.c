/*
 * animal.c
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "animal.h"
#include "animal_p.h"

void __animal_sound(struct animal_t *animal)
{
	printf("%s: Beeeep!\n", animal->name);
}

struct animal_t *animal_new()
{
	return (struct animal_t *)malloc(sizeof(struct animal_t));
}

void animal_init(struct animal_t *animal)
{
	animal->name = malloc(10 * sizeof(char));
	snprintf(animal->name, 10, "%s", "Animal");
	animal->sound_function = __animal_sound;
}

void animal_destroy(struct animal_t *animal)
{
	free(animal->name);
}

void animal_get_name(struct animal_t *animal, char *buf)
{
	snprintf(buf, 10, "%s", animal->name);
}

void animal_sound(struct animal_t *animal)
{
	animal->sound_function(animal);
}
