#include "car.h"

#include <stdlib.h>

struct car_t {
	struct engine_t *engine;
};

struct car_t *car_new() {
	struct car_t *car;
	car = malloc(sizeof(struct car_t));
	return car;
}

void car_init(struct car_t *car) {
	car->engine = engine_new();
	engine_init(car->engine);
}

void car_destroy(struct car_t *car) {
	engine_destroy(car->engine);
	/* Important! Free the memory allocated for the engine !*/
	free(car->engine);
}

void car_start(struct car_t *car) { engine_turn_on(car->engine); }
void car_stop(struct car_t *car) { engine_turn_off(car->engine); }

double car_get_engine_temperature(struct car_t *car) {
	return engine_get_temperature(car->engine);
}
