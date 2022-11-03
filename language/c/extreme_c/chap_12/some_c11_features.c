/*
 * some_c11_features.c
 */

#include <stdio.h>
#include <uchar.h>

/* Generic Macro */
#define abs(x) _Generic((x), int : absi, double : absd)(x)

int absi(int a) { return a > 0 ? a : -a; }

double absd(double a) { return a > 0 ? a : -a; }

/* Anonymous unions and structs */
struct point_t {
	union {
		struct {
			int x;
			int y;
		};
		int data[2];
	};
};

int main(int argc, char *argv[]) {
	/* Generic Macro */
	int a = -2;
	double b = -2.5;
	printf("The abs of %d is %d\n", a, abs(a));
	printf("The abs of %f is %f\n", b, abs(b));

	/* Anonymous unions and structs */
	struct point_t p;
	p.x = 10;
	p.data[1] = -5;
	printf("Point using an anonymous structure: (%d, %d)\n", p.x, p.y);
	printf("Point using byte array structure: (%d, %d)\n", p.data[0],
		   p.data[1]);

	return 0;
}
