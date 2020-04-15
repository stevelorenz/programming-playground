/*
 * car.h
 * Basic example of using implicit encapsulation in C for OOP.
 */

#ifndef CAR_H
#define CAR_H

#define CAR_NAME_LEN 32

/**
 * @brief The struct contains all attributes.
 */
struct car_t {
	char name[CAR_NAME_LEN];
	double speed;
	double fuel;
};

/* Behavior of the object are a set of functions. */
void car_construct(struct car_t *car, const char *name);
void car_destruct(struct car_t *car);
void car_accelerate(struct car_t *car);
void car_brake(struct car_t *car);
void car_refuel(struct car_t *car, double amount);

#endif /* !CAR_H */
