#include <stdio.h>
#include <stdlib.h>

#include "animal.h"
#include "cat.h"

int main(int argc, char *argv[]) {
	struct animal_t *animal = NULL;
	struct cat_t *cat = NULL;

	animal = animal_new();
	cat = cat_new();

	animal_init(animal);
	animal_sound(animal);
	cat_init(cat);
	animal_sound((struct animal_t *)cat);

	animal_destroy(animal);
	cat_destroy(cat);
	free(animal);
	free(cat);

	return 0;
}
