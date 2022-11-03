#include <stdio.h>

#define CONDITION

int main(int argc, char *argv[]) {
#ifdef CONDITION
	int i = 0;
	i++;
#endif
	int j = 0;
	j++;
	return 0;
}
