#include <stdio.h>
#include <stdlib.h>

#include "person.h"
#include "student.h"

int main(void)
{
	struct person_t *person;
	struct student_t *student;
	char first_name[32];
	char last_name[32];
	char student_number[16];

	student = student_new();
	student_init(student, "FOOS", "FOOS", 1991, "123", 17);
	person = (struct person_t *)student;
	person_get_first_name(person, first_name);
	person_get_first_name(person, last_name);
	printf("The name of the student is: %s %s.\n", first_name, last_name);
	student_get_student_number(student, student_number);
	printf("The student number is %s.\n", student_number);

	person_destroy(person);
	student_destroy(student);
	// ERROR: Double-free! person and student point to the same address.
	// free(person);
	free(student);

	return 0;
}
