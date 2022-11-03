/*
 * person.h
 */

#ifndef PERSON_H
#define PERSON_H

struct person_t;

struct person_t *person_new();
void person_init(struct person_t *person, const char *first_name,
				 const char *last_name, unsigned int birth_year);
void person_destroy(struct person_t *person);

void person_get_first_name(struct person_t *person, char *result);
void person_get_last_name(struct person_t *person, char *result);
unsigned int person_get_birth_year(struct person_t *person);

#endif /* !PERSON_H */
