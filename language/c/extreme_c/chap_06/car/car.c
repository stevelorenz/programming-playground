/*
 * car.c
 */

#include <string.h>

#include "car.h"

void car_construct(struct car_t *car, const char *name)
{
	strncpy(car->name, name, CAR_NAME_LEN);
	car->speed = 0.0;
	car->fuel = 0.0;
}

void car_destruct(struct car_t *car)
{
}

void car_accelerate(struct car_t *car)
{
	car->speed += 0.05;
	car->fuel -= 1.0;
	if (car->fuel < 0) {
		car->fuel = 0.0;
	}
}
void car_brake(struct car_t *car)
{
	car->speed -= 0.07;
	if (car->speed < 0) {
		car->speed = 0;
	}
	car->fuel -= 2.0;
	if (car->fuel < 0) {
		car->fuel = 0.0;
	}
}

void car_refuel(struct car_t *car, double amount)
{
	car->fuel = amount;
}
