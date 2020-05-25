/*
 * seamlessone.c
 */

#include <math.h>
#include <stdio.h>

struct point {
	double x;
	double y;
};

struct threepoint {
	// Use an union to let the anonymous struct and the named struct share
	// the same memory.
	union {
		// Use anonymous struct
		struct point;
		struct point p2;
	};
	double z;
};

double distance_2p(struct point p)
{
	return sqrt(p.x * p.x + p.y + p.y);
}

double distance_3p(struct threepoint p)
{
	return sqrt(p.x * p.x + p.y * p.y + p.z * p.z);
}

int main(void)
{
	struct threepoint p = { .x = 3, .y = 2, .z = 1 };
	printf("The distance of point p: %g\n", distance_3p(p));
	double distance_xy = distance_2p(p.p2);
	printf("The distance of point p on XY plane: %g\n", distance_xy);
	return 0;
}
