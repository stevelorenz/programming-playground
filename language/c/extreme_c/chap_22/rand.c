/*
 * rand.c
 */

#include <stdio.h>
#include <stdlib.h>

#include "rand.h"

bool_t random_boolean()
{
	int number = rand();
	return (number % 2);
}
