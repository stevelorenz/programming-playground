#include "curr_time.h"

#include <time.h>

#define BUF_SIZE 1000

char* currTime(const char* format) {
	static char buf[BUF_SIZE];
	time_t t;
	size_t s;
	struct tm* tm;

	t = time(NULL);
	tm = localtime(&t);
	if (tm == NULL) {
		return NULL;
	}

	s = strftime(buf, BUF_SIZE, (format != NULL) ? format : "%c", tm);

	if (s == 0) {
		return NULL;
	}
	return buf;
}

int main() { return 0; }
