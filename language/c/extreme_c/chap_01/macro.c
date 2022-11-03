/*
 * macro.c
 */

#include <stdio.h>
#include <string.h>

/* 1. function macro */
#define ADD(x, y) ((x) + (y))

#define PRINT(d) printf("%d\n", d);

/* 2. how to generate loop using macro */
#define LOOP(v, s, e) for (v = s; v <= e; v++) {
#define ENDLOOP }

/* 3. using ## and # for macro variables */
#define CMD(NAME)              \
	char NAME##_cmd[256] = ""; \
	strcpy(NAME##_cmd, #NAME);

/* 4. Variadic macro */
#define LOG_ERROR(format, ...) fprintf(stderr, format, __VA_ARGS__)

int main(int argc, char *argv[]) {
	int x = 2;
	int y = 3;
	int z = ADD(x, y);
	printf("Z is %d\n", z);

	int counter = 0;
	LOOP(counter, 1, 5)
	ENDLOOP
	PRINT(counter)

	CMD(copy)
	CMD(cut)
	char cmd[256];
	scanf("%s", cmd);
	if (strcmp(cmd, copy_cmd) == 0) {
		printf("This is a copy command!\n");
	} else if (strcmp(cmd, cut_cmd) == 0) {
		printf("This is a cut command\n");
	} else {
		LOG_ERROR("Error: \"%s\" is an invalid command\n", cmd);
	}

	return 0;
}
