/*
 * calc_proto_req.h
 */

#ifndef CALC_PROTO_REQ_H
#define CALC_PROTO_REQ_H

#include <stdint.h>

enum method_t { NONE, GETMEM, RESMEM, ADD, ADDM, SUB, SUBM, MUL, MULM, DIV };

struct calc_proto_req_t {
	int32_t id;
	enum method_t method;
	double operand1;
	double operand2;
};

enum method_t str_to_method(const char *str);
const char *method_to_str(enum method_t method);

#endif /* !CALC_PROTO_REQ_H */
