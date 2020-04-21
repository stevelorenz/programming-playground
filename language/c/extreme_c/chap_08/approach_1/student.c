#include <stdio.h>
#include <stdlib.h>

#include "person.h"
#include "person_p.h"

struct student_t {
	struct person_t person;
	// Extra attributes
	char student_number[16];
	unsigned int passed_credits;
};

struct student_t *student_new()
{
	return (struct student_t *)malloc(sizeof(struct student_t));
}

void student_init(struct student_t *student, const char *first_name,
		  const char *last_name, unsigned int birth_year,
		  const char *student_number, unsigned int passed_credits)
{
	// Call the constructor of the parent class.
	person_init((struct person_t *)student, first_name, last_name,
		    birth_year);
	snprintf(student->student_number, 16, "%s", student_number);
	student->passed_credits = passed_credits;
}

void student_destroy(struct student_t *student)
{
	person_destroy((struct person_t *)student);
}
//
void student_get_student_number(struct student_t *student, char *result)
{
	snprintf(result, 16, "%s", student->student_number);
}

unsigned int student_get_passed_credits(struct student_t *student)
{
	return student->passed_credits;
}
