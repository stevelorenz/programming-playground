#include <stdio.h>
#include <stdlib.h>

struct parent_t {
	char c;
	char d;
};

struct child_t {
	struct parent_t parent;
	char str[5];
};

int main(int argc, char *argv[]) {
	struct child_t c;
	c.parent.c = 'A';
	c.parent.d = 'B';
	snprintf(c.str, 5, "%s", "1234");
	return 0;
}
