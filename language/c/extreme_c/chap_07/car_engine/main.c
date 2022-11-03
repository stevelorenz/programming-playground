#include <stdio.h>
#include <stdlib.h>

#include "car.h"

int main(int argc, char *argv[]) {
	struct car_t *car;
	car = car_new();
	car_init(car);
	printf("Engine temperature before starting the car: %.2f.\n",
		   car_get_engine_temperature(car));

	car_start(car);
	printf("Engine temperature after starting the car: %.2f.\n",
		   car_get_engine_temperature(car));

	car_stop(car);
	printf("Engine temperature after stopping the car: %.2f.\n",
		   car_get_engine_temperature(car));

	car_destroy(car);
	free(car);

	return 0;
}
