/*
 * linker_test.c
 */

int average(int a, int b)
{
	return (a + b) / 2;
}

int sum(int *numbers, int count)
{
	int i, sum = 0;
	for (i = 0; i < count; ++i) {
		sum += numbers[i];
	}
	return sum;
}
