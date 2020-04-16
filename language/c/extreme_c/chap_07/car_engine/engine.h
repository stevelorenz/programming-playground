/*
 * engine.h
 */

#ifndef ENGINE_H
#define ENGINE_H

/*! \enum state_t
 *
 *  The state of the engine.
 */
enum state_t { ON, OFF };

struct engine_t;

struct engine_t *engine_new();
void engine_init(struct engine_t *engine);
void engine_destroy(struct engine_t *engine);

void engine_turn_on(struct engine_t *engine);
void engine_turn_off(struct engine_t *engine);
double engine_get_temperature(struct engine_t *engine);

#endif /* !ENGINE_H */
