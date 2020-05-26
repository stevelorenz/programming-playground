/*
 * quicksort.c
 */

#include <stdio.h>
#include <stdlib.h>

void swap(int v[], int i, int j)
{
	int temp;
	temp = v[i];
	v[i] = v[j];
	v[j] = temp;
}

void quicksort(int v[], int n)
{
	if (n <= 1) {
		return;
	}
	// Move the pivot element to v[0]
	swap(v, 0, rand() % n);

	int i = 0;
	int last = 0;

	for (i = 1; i < n - 1; ++i) {
		if (v[i] < v[0]) {
			swap(v, ++last, i);
		}
	}
	// Restore the pivot element.
	swap(v, 0, last);

	// Recursively sort the subsets.
	quicksort(v, last);
	quicksort(v + last + 1, n - last - 1);
}

void print_array(int v[], int n)
{
	int i = 0;
	for (i = 0; i < n; ++i) {
		printf("%d,", v[i]);
	}
	printf("\n");
}

int main()
{
	int i = 0;
	int numbers[30];
	for (i = 0; i < 30; ++i) {
		numbers[i] = rand() % 30;
	}
	printf("Array before sorting: \n");
	print_array(numbers, 30);
	quicksort(numbers, 30);
	printf("Array after sorting: \n");
	print_array(numbers, 30);
	return 0;
}
