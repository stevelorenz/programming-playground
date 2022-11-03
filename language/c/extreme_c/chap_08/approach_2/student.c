#include "student.h"

#include <stdio.h>
#include <stdlib.h>

#include "person.h"

struct student_t {
	// A pointer to person_t is used here since the type of the person_t is
	// incomplete here.
	struct person_t *person;
	// Extra attributes
	char student_number[16];
	unsigned int passed_credits;
};

struct student_t *student_new() {
	return (struct student_t *)malloc(sizeof(struct student_t));
}

void student_init(struct student_t *student, const char *first_name,
				  const char *last_name, unsigned int birth_year,
				  const char *student_number, unsigned int passed_credits) {
	// Allocate memory for the parent class.
	student->person = person_new();
	person_init(student->person, first_name, last_name, birth_year);

	snprintf(student->student_number, 16, "%s", student_number);
	student->passed_credits = passed_credits;
}

void student_destroy(struct student_t *student) {
	person_destroy(student->person);
	// Free the memory allocated for the parent object.
	free(student->person);
}

void student_get_first_name(struct student_t *student, char *result) {
	// Call the behavior function of the parent class.
	person_get_first_name(student->person, result);
}

void student_get_last_name(struct student_t *student, char *result) {
	person_get_last_name(student->person, result);
}

unsigned int student_get_birth_year(struct student_t *student) {
	return person_get_birth_year(student->person);
}

void student_get_student_number(struct student_t *student, char *result) {
	snprintf(result, 16, "%s", student->student_number);
}

unsigned int student_get_passed_credits(struct student_t *student) {
	return student->passed_credits;
}
