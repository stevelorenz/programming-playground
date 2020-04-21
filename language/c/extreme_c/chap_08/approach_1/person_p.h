/*
 * person_p.h
 *
 * The private header for the person class.
 */

#ifndef PERSON_P_H
#define PERSON_P_H

struct person_t {
	char first_name[32];
	char last_name[32];
	unsigned int birth_year;
};

#endif /* !PERSON_P_H */
