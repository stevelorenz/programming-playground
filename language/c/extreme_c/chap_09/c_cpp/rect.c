#include <stdio.h>

struct rect_t {
	int width;
	int length;
};

int rect_area(struct rect_t *rect)
{
	return (rect->width * rect->length);
}

int main(int argc, char *argv[])
{
	struct rect_t r;
	r.width = 10;
	r.length = 25;

	int area = rect_area(&r);
	printf("The area : %d\n", area);

	return 0;
}
