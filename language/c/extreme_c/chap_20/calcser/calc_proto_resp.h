/*
 * calc_proto_resp.h
 */

#include <stdint.h>

#ifndef CALC_PROTO_RESP_H
#define CALC_PROTO_RESP_H

#define STATUS_OK 0
#define STATUS_INVALID_REQUEST 1
#define STATUS_INVALID_METHOD 2
#define STATUS_INVALID_OPERAND 3
#define STATUS_DIV_BY_ZERO 4
#define STATUS_INTERNAL_ERROR 20

struct calc_proto_resp_t {
	int32_t resp_id;
	int status;
	double result;
};

#endif /* !CALC_PROTO_RESP_H */
