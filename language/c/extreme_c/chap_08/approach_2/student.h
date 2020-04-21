/*
 * student.h
 */

#ifndef STUDENT_H
#define STUDENT_H

struct student_t *student;

struct student_t *student_new();
void student_init(struct student_t *student, const char *first_name,
		  const char *last_name, unsigned int birth_year,
		  const char *student_number, unsigned int passed_credits);
void student_destroy(struct student_t *student);

void student_get_first_name(struct student_t *student, char *result);
void student_get_last_name(struct student_t *student, char *result);
unsigned int student_get_birth_year(struct student_t *student);

void student_get_student_number(struct student_t *student, char *result);
unsigned int student_get_passed_credits(struct student_t *student);

#endif /* !STUDENT_H */
