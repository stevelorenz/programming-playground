/*
 * rand.c
 */

#include "rand.h"

#include <stdio.h>
#include <stdlib.h>

bool_t random_boolean() {
	int number = rand();
	return (number % 2);
}
