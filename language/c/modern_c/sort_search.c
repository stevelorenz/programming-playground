/*
 * Example of using qsort and bsearch in stdlib.
 * */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct mi {
	int nr;
	char *name;
} months[] = { { 1, "jan" }, { 2, "feb" },  { 3, "mar" },  { 4, "apr" },
	       { 5, "may" }, { 6, "jun" },  { 7, "jul" },  { 8, "aug" },
	       { 9, "sep" }, { 10, "oct" }, { 11, "nov" }, { 12, "dec" } };

#define NUM_MONTHS (sizeof(months) / sizeof(months[0]))

static int compmi(const void *m1, const void *m2)
{
	struct mi *mi1 = (struct mi *)m1;
	struct mi *mi2 = (struct mi *)m2;
	return strcmp(mi1->name, mi2->name);
}

int main(int argc, char *argv[])
{
	size_t i;
	qsort(months, NUM_MONTHS, sizeof(struct mi), compmi);
	printf("The month after alphabet order:");
	for (i = 0; i < NUM_MONTHS; ++i) {
		printf("%s, ", months[i].name);
	}
	printf("\n");

	struct mi key = { .name = "aug" };
	struct mi *res;
	res = bsearch(&key, months, NUM_MONTHS, sizeof(struct mi), compmi);
	if (res == NULL) {
		printf("%s: unknown month.\n", key.name);
	} else {
		printf("%s: month #%d\n", res->name, res->nr);
	}
	return EXIT_SUCCESS;
}
