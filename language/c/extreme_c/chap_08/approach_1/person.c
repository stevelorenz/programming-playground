#include "person.h"

#include <stdio.h>
#include <stdlib.h>

#include "person_p.h"

struct person_t *person_new() {
	return (struct person_t *)malloc(sizeof(struct person_t));
}

void person_init(struct person_t *person, const char *first_name,
				 const char *last_name, unsigned int birth_year) {
	snprintf(person->first_name, 32, "%s", first_name);
	snprintf(person->last_name, 32, "%s", last_name);
	person->birth_year = birth_year;
}

void person_destroy(struct person_t *person) {}

void person_get_first_name(struct person_t *person, char *result) {
	snprintf(result, 32, "%s", person->first_name);
}
void person_get_last_name(struct person_t *person, char *result) {
	snprintf(result, 32, "%s", person->last_name);
}
/* unsigned int person_get_birth_year(struct person_t *person); */
