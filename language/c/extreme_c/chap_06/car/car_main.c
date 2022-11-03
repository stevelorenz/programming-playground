/*
 * car_main.c
 */

#include <stdio.h>

#include "car.h"

int main(int argc, char *argv[]) {
	struct car_t car;
	car_construct(&car, "fancy car");
	car_refuel(&car, 100.0);
	printf("Car is refueled, the current fuel value is: %.1f\n", car.fuel);

	while (car.fuel > 0) {
		if (car.speed < 80) {
			car_accelerate(&car);
			printf("Car has been accelerated to the speed: %.2f\n", car.speed);
		} else {
			car_brake(&car);
			printf("Car has been slow down to the speed: %.2f\n", car.speed);
		}
	}
	printf("Car runs out of the fuel! Slowing down...\n");
	while (car.speed > 0) {
		car_brake(&car);
		printf("Car has been slow down to the speed: %.2f\n", car.speed);
	}

	car_destruct(&car);
	return 0;
}
