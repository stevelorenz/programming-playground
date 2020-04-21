/*
 * cat.c
 */

#include <stdio.h>
#include <stdlib.h>

#include "cat.h"
#include "animal.h"
#include "animal_p.h"

struct cat_t {
	struct animal_t animal;
};

// Define a new hehavior function for the cat's sound.

void __cat_sound(struct animal_t *animal)
{
	printf("%s: Meow!\n", animal->name);
}

struct cat_t *cat_new()
{
	return malloc(sizeof(struct cat_t));
}

void cat_init(struct cat_t *cat)
{
	animal_init((struct animal_t *)cat);
	snprintf(cat->animal.name, 10, "%s", "Cat");
	// Point to the new behavior function! Here is where the overriding
	// happens!
	cat->animal.sound_function = __cat_sound;
}

void cat_destroy(struct cat_t *cat)
{
	animal_destroy((struct animal_t *)cat);
}
