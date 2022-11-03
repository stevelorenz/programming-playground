/*
 * Simple example to show a cache-friendly algorithm.
 *
 * Run sum with a matrix of 10000 rows and 10000 columns can show siginificant
 * run time difference.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void fill_matrix(int *matrix, int rows, int columns) {
	int i = 0;
	int j = 0;
	int counter = 1;
	for (i = 0; i < rows; i++) {
		for (j = 0; j < columns; j++) {
			*(matrix + i * columns + j) = counter;
		}
		counter++;
	}
}

void print_matrix(int *matrix, int rows, int columns) {
	int i = 0;
	int j = 0;
	for (i = 0; i < rows; i++) {
		for (j = 0; j < columns; j++) {
			printf("%d, ", *(matrix + i * columns + j));
		}
		printf("\n");
	}
}

int cache_friendly_sum(int *matrix, int rows, int columns) {
	int i = 0;
	int j = 0;
	int sum = 0;
	for (i = 0; i < rows; ++i) {
		for (j = 0; j < columns; ++j) {
			sum += *(matrix + i * columns + j);
		}
	}
	return sum;
}

int cache_unfriendly_sum(int *matrix, int rows, int columns) {
	int i = 0;
	int j = 0;
	int sum = 0;
	for (j = 0; j < columns; ++j) {
		for (i = 0; i < rows; ++i) {
			sum += *(matrix + i * columns + j);
		}
	}
	return sum;
}

int get_operation_code(char *operation) {
	int i = 0;
	char *valid_operations[] = {"print", "friendly-sum", "not-friendly-sum"};
	for (i = 0; i < 3; ++i) {
		if (strncmp(operation, valid_operations[i],
					strlen(valid_operations[i])) == 0) {
			return i;
		}
	}
	return -1;
}

int main(int argc, char *argv[]) {
	if (argc < 4) {
		printf(
			"Usage: %s [print|friendly-sum|not-friendly-sum] [number-of-rows] "
			"[number-of-columns]\n",
			argv[0]);
		exit(1);
	}

	char *operation = argv[1];
	int rows = atol(argv[2]);
	int columns = atol(argv[3]);

	int opt_code = -2;
	opt_code = get_operation_code(operation);
	if (opt_code == -1) {
		printf("Error: Invalid operation!\n");
	}
	int *matrix = NULL;
	matrix = malloc(rows * columns * sizeof(int));
	fill_matrix(matrix, rows, columns);

	if (opt_code == 0) {
		print_matrix(matrix, rows, columns);
	} else if (opt_code == 1) {
		int sum = 0;
		sum = cache_friendly_sum(matrix, rows, columns);
		printf("[Friendly] The sum of the matrix: %d\n", sum);
	} else if (opt_code == 2) {
		int sum = 0;
		sum = cache_unfriendly_sum(matrix, rows, columns);
		printf("[Unfriendly] The sum of the matrix: %d\n", sum);
	}

	free(matrix);

	return 0;
}
