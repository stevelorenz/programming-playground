#include <stdlib.h>

#include "engine.h"

struct engine_t {
	enum state_t state;
	double temperature;
};

struct engine_t *engine_new()
{
	return (struct engine_t *)malloc(sizeof(struct engine_t));
}

void engine_init(struct engine_t *engine)
{
	engine->state = OFF;
	engine->temperature = 15.0;
}
void engine_destroy(struct engine_t *engine)
{
}

void engine_turn_on(struct engine_t *engine)
{
	if (engine->state == ON) {
		return;
	}
	engine->state = ON;
	engine->temperature = 75.0;
}

void engine_turn_off(struct engine_t *engine)
{
	if (engine->state == OFF) {
		return;
	}
	engine->state == OFF;
	engine->temperature = 15.0;
}

double engine_get_temperature(struct engine_t *engine)
{
	return engine->temperature;
}
