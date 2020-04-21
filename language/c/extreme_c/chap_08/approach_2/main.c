#include <stdio.h>
#include <stdlib.h>

#include "student.h"

int main(void)
{
	struct student_t *student;
	char first_name[32];
	char last_name[32];
	char student_number[16];

	student = student_new();
	student_init(student, "FOOS", "FOOS", 1991, "123", 17);
	student_get_first_name(student, first_name);
	student_get_first_name(student, last_name);
	printf("The name of the student is: %s %s.\n", first_name, last_name);
	student_get_student_number(student, student_number);
	printf("The student number is %s.\n", student_number);

	student_destroy(student);
	free(student);

	return 0;
}
