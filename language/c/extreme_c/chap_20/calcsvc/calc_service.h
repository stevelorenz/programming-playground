/*
 * calc_service.h
 */

#ifndef CALC_SERVICE_H
#define CALC_SERVICE_H

#include <types.h>

static const int CALC_SVC_OK = 0;
static const int CALC_SVC_ERROR_DIV_BY_ZERO = -1;

struct calc_service_t;

struct calc_service_t *calc_service_new();
void calc_service_delete(struct calc_service_t *svc);

void calc_service_constructor(struct calc_service_t *svc);
void calc_service_destructor(struct calc_service_t *svc);

void calc_service_reset_mem(struct calc_service_t *svc);
double calc_service_get_mem(struct calc_service_t *svc);
double calc_service_add(struct calc_service_t *svc, double a, double b,
			bool_t mem);
double calc_service_sub(struct calc_service_t *svc, double a, double b,
			bool_t mem);

double calc_service_mul(struct calc_service_t *svc, double a, double b,
			bool_t mem);

int calc_service_div(struct calc_service_t *svc, double a, double b,
		     double *result);

#endif /* !CALC_SERVICE_H */
