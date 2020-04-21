/*
 * animal.h
 */

#ifndef ANIMAL_H
#define ANIMAL_H

struct animal_t;

struct animal_t *animal_new();

void animal_init(struct animal_t *animal);
void animal_destroy(struct animal_t *animal);

void animal_get_name(struct animal_t *animal, char *buf);
void animal_sound(struct animal_t *animal);

#endif /* !ANIMAL_H */
