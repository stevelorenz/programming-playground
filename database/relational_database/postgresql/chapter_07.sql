-- Use column constraint
DROP TABLE IF EXISTS natural_key_example;
CREATE TABLE IF NOT EXISTS natural_key_example (
	license_id varchar(10) CONSTRAINT license_key PRIMARY KEY,
	first_name varchar(50),
	last_name varchar(50)
);

-- Use table constraint
DROP TABLE natural_key_example;
CREATE TABLE IF NOT EXISTS natural_key_example (
	license_id varchar(10),
	first_name varchar(50),
	last_name varchar(50),
	CONSTRAINT license_key PRIMARY KEY (license_id)
);

INSERT INTO natural_key_example (license_id, first_name, last_name)
VALUES ('T229901', 'Lynn', 'Malero');
DROP TABLE natural_key_example;

-- Error! the license_id is a primary key, duplicated!
-- INSERT INTO natural_key_example (license_id, first_name, last_name)
-- VALUES ('T229901', 'Sam', 'Tracy');

-- Use composite key -> a key
DROP TABLE IF EXISTS natural_key_composite_example;
CREATE TABLE IF NOT EXISTS natural_key_composite_example(
	student_id varchar(10),
	school_day date,
	present boolean,
	CONSTRAINT student_key PRIMARY KEY (student_id, school_day)
);

INSERT INTO natural_key_composite_example (student_id, school_day, present)
VALUES (775, '1/22/2017', 'Y');

INSERT INTO natural_key_composite_example (student_id, school_day, present)
VALUES (775, '1/23/2017', 'Y');

-- This will violate the composite primary key!
-- INSERT INTO natural_key_composite_example (student_id, school_day, present)
-- VALUES (775, '1/23/2017', 'N');

SELECT *
FROM natural_key_composite_example;

DROP TABLE natural_key_composite_example;

DROP TABLE IF EXISTS surrogate_key_example;
CREATE TABLE IF NOT EXISTS surrogate_key_example (
	order_number bigserial,
	product_name varchar(50),
	order_date date,
	CONSTRAINT order_key PRIMARY KEY(order_number)
);

INSERT INTO surrogate_key_example (product_name, order_date)
VALUES
	('Beachball Polish', '2015-03-17'),
	('Wrinkle De-Atomizer', '2017-05-22'),
	('Flux Capacitor', '1985-10-26');

SELECT * FROM surrogate_key_example
ORDER BY order_date DESC;

DROP TABLE surrogate_key_example;

-- An example to use the foreign key
DROP TABLE IF EXISTS licenses;
CREATE TABLE IF NOT EXISTS licenses (
	license_id varchar(10),
	first_name varchar(50),
	last_name varchar(50),
	CONSTRAINT license_key PRIMARY KEY (license_id)
);

DROP TABLE IF EXISTS registration;
CREATE TABLE IF NOT EXISTS registration (
	registration_id varchar(10),
	registration_date date,
	license_id varchar(10) REFERENCES licenses (license_id) ON DELETE CASCADE, -- Add reference to a column in another table
	CONSTRAINT registration_key PRIMARY KEY (registration_id, license_id)
);

INSERT INTO licenses (license_id, first_name, last_name)
VALUES ('T229901', 'Lynn', 'Malero');

-- This works because T229901 already exists in licenses table
INSERT INTO registration (registration_id, registration_date, license_id)
VALUES ('A203391', '3/17/2017', 'T229901');

-- This insert will raise error because T000001 is not in licenses table
-- INSERT INTO registration (registration_id, registration_date, license_id)
-- VALUES ('A75772', '3/17/2017', 'T000001');

-- The table registration must be removed before licenses, because registration has a reference to licenses.
DROP TABLE registration;
DROP TABLE licenses;

-- Examples for CHECK constraints

DROP TABLE IF EXISTS check_constraint_example;
CREATE TABLE IF NOT EXISTS check_constraint_example (
	user_id bigserial,
	user_role varchar(50),
	salary integer,
	CONSTRAINT user_id_key PRIMARY KEY (user_id),
	CONSTRAINT check_role_in_list CHECK (user_role IN ('Admin', 'Staff')),
	CONSTRAINT check_salary_not_zero CHECK (salary > 0)
);

-- Both of following will fail
INSERT INTO check_constraint_example (user_role)
VALUES ('admin');
INSERT INTO check_constraint_example (salary)
VALUES (0);

DROP TABLE check_constraint_example;

DROP TABLE IF EXISTS unique_constraint_example;
CREATE TABLE IF NOT EXISTS unique_constraint_example (
    contact_id bigserial,
    first_name varchar(50) NOT NULL,
    last_name varchar(50) NOT NULL,
    email varchar(200) NOT NULL,
	CONSTRAINT contact_id_key_const PRIMARY KEY (contact_id),
	CONSTRAINT email_unique_const UNIQUE (email)
);

INSERT INTO unique_constraint_example (first_name, last_name, email)
VALUES ('Samantha', 'Lee', 'slee@example.org');

INSERT INTO unique_constraint_example (first_name, last_name, email)
VALUES ('Betty', 'Diaz', 'bdiaz@example.org');

-- Raise error because the email already exists
INSERT INTO unique_constraint_example (first_name, last_name, email)
VALUES ('Sasha', 'Lee', 'slee@example.org');

-- Remove the constraint and insert one item
ALTER TABLE unique_constraint_example DROP CONSTRAINT email_unique_const;
INSERT INTO unique_constraint_example (first_name, last_name, email)
VALUES ('Sasha', 'Lee', 'slee@example.org');
 
DROP TABLE unique_constraint_example;

DROP TABLE IF EXISTS new_york_addresses;
CREATE TABLE IF NOT EXISTS new_york_addresses (
    longitude numeric(9,6),
    latitude numeric(9,6),
    street_number varchar(10),
    street varchar(32),
    unit varchar(7),
    postcode varchar(5),
    id integer CONSTRAINT new_york_key PRIMARY KEY -- The id has a index
);

-- This import takes some time, more than 900,000 entires
COPY new_york_addresses
FROM '/tmp/city_of_new_york.csv'
WITH (FORMAT CSV, HEADER);

-- Benchmark queries for large table
EXPLAIN ANALYZE SELECT * FROM new_york_addresses
WHERE street = 'BROADWAY';

-- Create a B-Tree index on the new_york_addresses table
CREATE INDEX street_idx ON new_york_addresses (street);
EXPLAIN ANALYZE SELECT * FROM new_york_addresses
WHERE street = 'BROADWAY';

DROP TABLE new_york_addresses;
