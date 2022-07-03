-- Query all rows and columns from the table teachers
SELECT * FROM teachers;

-- Query subset of columns
SELECT last_name, first_name, salary FROM teachers;

-- Query unique values in a given column
SELECT DISTINCT school FROM teachers;

SELECT DISTINCT school, salary FROM teachers;

-- Sort a column, ASC and DESC
SELECT first_name, last_name, salary
FROM teachers
ORDER BY salary DESC;

-- Sort multiple columns
SELECT last_name, school, hire_date
FROM teachers
ORDER BY school ASC, hire_date DESC;

-- Use where to filter rows
SELECT last_name, school, salary
FROM teachers
WHERE school = 'Myers Middle School';

SELECT last_name, school, salary
FROM teachers
WHERE school <> 'Myers Middle School';

SELECT last_name, school, hire_date, salary
FROM teachers
WHERE hire_date < '2000-01-01';

SELECT last_name, school, salary
FROM teachers
WHERE salary BETWEEN 40000 AND 65000;

SELECT first_name
FROM teachers
WHERE first_name LIKE 'sam%';

-- ILIKE is case insensitive
SELECT first_name
FROM teachers
WHERE first_name ILIKE 'sam%';

SELECT *
FROM teachers
WHERE school = 'Myers Middle School'
	AND salary < 40000;

SELECT first_name, last_name, school, hire_date, salary
FROM teachers
WHERE school ILIKE '%Roos%'
ORDER BY hire_date DESC;
