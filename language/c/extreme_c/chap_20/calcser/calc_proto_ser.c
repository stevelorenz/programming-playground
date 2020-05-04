/*
 * calc_proto_ser.c
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "calc_proto_ser.h"

#define FIELD_COUNT_PER_REQ_MESSAGE 4
#define FIELD_COUNT_PER_RESP_MESSAGE 3
#define MESSAGE_DELIMITER '$'
#define FIELD_DELIMITER '#'

struct calc_proto_ser_t {
	char *ring_buf;
	int buf_len;
	int cur_idx;
	int start_idx;
	void (*error_cb)(void *owner_obj, const int req_id,
			 const int error_code);
	void (*req_cb)(void *owner_obj, struct calc_proto_req_t req);
	void (*resp_cb)(void *owner_obj, struct calc_proto_resp_t resp);
	void *context;
};

void _serialize_double(char *str, double num)
{
	char tmp[64];
	sprintf(tmp, "%lf", num);
	strcpy(str, tmp);
	int i = strlen(str) - 1;
	while (str[i] == '0' && i >= 0)
		i--;
	if (i >= 0)
		str[i + 1] = '\0';
	if (i >= 0 && str[i] == '.')
		str[i] = '\0';
}

bool_t _is_buffer_full(struct calc_proto_ser_t *ser)
{
	if (ser->start_idx < 0) {
		return FALSE;
	}
}

/**
 * @brief Deserialize the data encapsulated in the buff struct.
 */
void _deserialize(struct calc_proto_ser_t *ser, struct buffer_t buff,
		  void (*parse_and_notify_func)(struct calc_proto_ser_t *ser),
		  int error_code, bool_t *found)
{
	// Check if the buff is valid.
	if (buff.len > ser->buf_len) {
		if (ser->error_cb) {
			ser->error_cb(ser->context, -1, error_code);
		}
		return;
	}
	bool_t overflow = FALSE;
	int i = 0;

	for (i = 0; i < buff.len; ++i) {
		ser->ring_buf[ser->cur_idx] = buff.data[i];
	}

	if (overflow) {
		_deserialize(ser, buff, parse_and_notify_func, error_code,
			     found);
	}
}

void _parse_req_and_notify(struct calc_proto_ser_t *ser)
{
}

struct calc_proto_ser_t *calc_proto_ser_new()
{
	return (struct calc_proto_ser_t *)malloc(
		sizeof(struct calc_proto_ser_t));
}

void calc_proto_ser_delete(struct calc_proto_ser_t *ser)
{
	free(ser);
}

void calc_proto_ser_constructor(struct calc_proto_ser_t *ser, void *owner_obj,
				int ring_buffer_size)
{
	ser->buf_len = ring_buffer_size;
	ser->ring_buf = (char *)malloc(ring_buffer_size * sizeof(char));
	ser->cur_idx = 0;
	ser->start_idx = -1;
	ser->req_cb = NULL;
	ser->resp_cb = NULL;
	ser->error_cb = NULL;

	ser->context = owner_obj;
}

void calc_proto_ser_destructor(struct calc_proto_ser_t *ser)
{
	free(ser->ring_buf);
}

void *calc_proto_ser_get_context(struct calc_proto_ser_t *ser)
{
	return ser->context;
}

void calc_proto_ser_set_req_callback(struct calc_proto_ser_t *ser,
				     void (*req_cb)(void *owner_obj,
						    struct calc_proto_req_t))
{
	ser->req_cb = req_cb;
}

void calc_proto_ser_set_resp_callback(struct calc_proto_ser_t *ser,
				      void (*resp_cb)(void *owner_obj,
						      struct calc_proto_resp_t))
{
	ser->resp_cb = resp_cb;
}

void calc_proto_ser_set_error_callback(struct calc_proto_ser_t *ser,
				       void (*error_cb)(void *owner_obj,
							const int req_id,
							const int error_code))
{
	ser->error_cb = error_cb;
}

void calc_proto_ser_server_deserialize(struct calc_proto_ser_t *ser,
				       struct buffer_t buff, bool_t *req_found)
{
	if (req_found) {
		*req_found = FALSE;
	}
	_deserialize(ser, buff, _parse_req_and_notify, ERROR_INVALID_REQUEST,
		     req_found);
}

struct buffer_t
calc_proto_ser_server_serialize(struct calc_proto_ser_t *ser,
				const struct calc_proto_resp_t *resp)
{
	struct buffer_t buff;
	char resp_result_str[64];
	_serialize_double(resp_result_str, resp->result);
	buff.data = (char *)malloc(64 * sizeof(char));
	snprintf(buff.data, 64, "%d%c%d%c%s%c", resp->resp_id, FIELD_DELIMITER,
		 (int)resp->status, FIELD_DELIMITER, resp_result_str,
		 MESSAGE_DELIMITER);
	// '\0' does not need to be transferred.
	buff.len = strlen(buff.data);

	return buff;
}
