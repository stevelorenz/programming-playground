/*
 * animal_p.h
 */

#ifndef ANIMAL_P_H
#define ANIMAL_P_H

struct animal_t {
	char *name;
	// This member is a function pointer to the function which performs the
	// actual sound behavior.
	void (*sound_function)(struct animal_t *);
};

#endif /* !ANIMAL_P_H */
