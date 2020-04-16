/*
 * car.h
 */

#ifndef CAR_H
#define CAR_H

#include "engine.h"

struct car_t;

struct car_t *car_new();
void car_init(struct car_t *car);
void car_destroy(struct car_t *car);

void car_start(struct car_t *car);
void car_stop(struct car_t *car);
double car_get_engine_temperature(struct car_t *car);

#endif /* !CAR_H */
