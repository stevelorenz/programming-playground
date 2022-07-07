DROP TABLE IF EXISTS departments;
CREATE TABLE IF NOT EXISTS departments (
	dept_id bigserial,
	dept varchar(100),
	city varchar(100),
	CONSTRAINT dept_key PRIMARY KEY (dept_id),  -- dept_id is the primary key
	CONSTRAINT dept_city_unique UNIQUE (dept, city)
);

DROP TABLE IF EXISTS employees;
CREATE TABLE IF NOT EXISTS employees (
	emp_id bigserial,
	first_name varchar(100),
	last_name varchar(100),
	salary integer,
	-- the dept_id is a foreign key connected to the dept_id column in departments table
	dept_id integer REFERENCES departments (dept_id), -- reference to the departments table
	CONSTRAINT emp_key PRIMARY KEY (emp_id),
	--
	CONSTRAINT emp_dept_unique UNIQUE (emp_id, dept_id)
);

INSERT INTO departments (dept, city)
VALUES
	('Tax', 'Atlanta'),
	('IT', 'Boston');

INSERT into employees (first_name, last_name, salary, dept_id)
VALUES
	('Nancy', 'Jones', 62500, 1),
	('Lee', 'Smith', 59300, 1),
	('Soo', 'Nguyen', 83000, 2),
	('Janet', 'King', 95000, 2);

SELECT *
FROM employees JOIN departments
ON employees.dept_id = departments.dept_id; -- SQL uses = instead of ==

-- Create two tables to explore different join types
DROP TABLE IF EXISTS schools_left;
DROP TABLE IF EXISTS schools_right;

CREATE TABLE IF NOT EXISTS schools_left (
	id integer CONSTRAINT left_id_key PRIMARY KEY,
	left_school varchar(30)
);

CREATE TABLE IF NOT EXISTS schools_right (
	id integer CONSTRAINT right_id_key PRIMARY KEY,
	right_school varchar(30)
);

INSERT INTO schools_left (id, left_school) VALUES
    (1, 'Oak Street School'),
    (2, 'Roosevelt High School'),
    (5, 'Washington Middle School'),
    (6, 'Jefferson High School');

INSERT INTO schools_right (id, right_school) VALUES
    (1, 'Oak Street School'),
    (2, 'Roosevelt High School'),
    (3, 'Morrison Elementary'),
    (4, 'Chase Magnet Academy'),
    (6, 'Jefferson High School');

SELECT *
FROM schools_left JOIN schools_right
ON schools_left.id = schools_right.id;

SELECT *
FROM schools_left LEFT JOIN schools_right
ON schools_left.id = schools_right.id;

SELECT *
FROM schools_left RIGHT JOIN schools_right
ON schools_left.id = schools_right.id;

SELECT *
FROM schools_left FULL OUTER JOIN schools_right
ON schools_left.id = schools_right.id;

SELECT *
FROM schools_left CROSS JOIN schools_right;

-- Show the rows that exist in schools_left but are missing in schools_right!
SELECT *
FROM schools_left LEFT JOIN schools_right
ON schools_left.id = schools_right.id
WHERE schools_right.id IS NULL;

-- Select specific columns in a join
SELECT schools_left.id, -- The table name must be given
	schools_left.left_school,
	schools_right.right_school
FROM schools_left LEFT JOIN schools_right
ON schools_left.id = schools_right.id;

-- Use alias to simplify the table name
SELECT lt.id, lt.left_school, rt.right_school
FROM schools_left AS lt LEFT JOIN schools_right as rt
ON lt.id = rt.id;

-- Join multiple tables
DROP TABLE IF EXISTS schools_enrollment;
CREATE TABLE IF NOT EXISTS schools_enrollment (
	id integer,
	enrollment integer
);

DROP TABLE IF EXISTS schools_grades;
CREATE TABLE IF NOT EXISTS schools_grades (
	id integer,
	grades varchar(10)
);

INSERT INTO schools_enrollment (id, enrollment)
VALUES
	(1, 360),
	(2, 1001),
	(5, 450),
	(6, 927);

INSERT INTO schools_grades (id, grades)
VALUES
	(1, 'K-3'),
	(2, '9-12'),
	(5, '6-8'),
	(6, '9-12');

-- Join multiple tables at once!
SELECT lt.id, lt.left_school, en.enrollment, gr.grades
FROM schools_left AS lt LEFT JOIN schools_enrollment AS en
	ON lt.id = en.id
LEFT JOIN schools_grades AS gr
	ON lt.id = gr.id;

DROP TABLE IF EXISTS us_counties_2000;
CREATE TABLE IF NOT EXISTS us_counties_2000 (
    geo_name varchar(90),              -- County/state name,
    state_us_abbreviation varchar(2),  -- State/U.S. abbreviation
    state_fips varchar(2),             -- State FIPS code
    county_fips varchar(3),            -- County code
    p0010001 integer,                  -- Total population
    p0010002 integer,                  -- Population of one race:
    p0010003 integer,                      -- White Alone
    p0010004 integer,                      -- Black or African American alone
    p0010005 integer,                      -- American Indian and Alaska Native alone
    p0010006 integer,                      -- Asian alone
    p0010007 integer,                      -- Native Hawaiian and Other Pacific Islander alone
    p0010008 integer,                      -- Some Other Race alone
    p0010009 integer,                  -- Population of two or more races
    p0010010 integer,                  -- Population of two races
    p0020002 integer,                  -- Hispanic or Latino
    p0020003 integer                   -- Not Hispanic or Latino:
);

-- Import the table from the CSV file
COPY us_counties_2000
FROM '/tmp/us_counties_2000.csv'
WITH (FORMAT CSV, HEADER);

DROP TABLE IF EXISTS us_counties_2010;
CREATE TABLE IF NOT EXISTS us_counties_2010 (
    geo_name varchar(90),                    -- Name of the geography
    state_us_abbreviation varchar(2),        -- State/U.S. abbreviation
    summary_level varchar(3),                -- Summary Level
    region smallint,                         -- Region
    division smallint,                       -- Division
    state_fips varchar(2),                   -- State FIPS code
    county_fips varchar(3),                  -- County code
    area_land bigint,                        -- Area (Land) in square meters
    area_water bigint,                        -- Area (Water) in square meters
    population_count_100_percent integer,    -- Population count (100%)
    housing_unit_count_100_percent integer,  -- Housing Unit count (100%)
    internal_point_lat numeric(10,7),        -- Internal point (latitude)
    internal_point_lon numeric(10,7),        -- Internal point (longitude)

    -- This section is referred to as P1. Race:
    p0010001 integer,   -- Total population
    p0010002 integer,   -- Population of one race:
    p0010003 integer,       -- White Alone
    p0010004 integer,       -- Black or African American alone
    p0010005 integer,       -- American Indian and Alaska Native alone
    p0010006 integer,       -- Asian alone
    p0010007 integer,       -- Native Hawaiian and Other Pacific Islander alone
    p0010008 integer,       -- Some Other Race alone
    p0010009 integer,   -- Population of two or more races
    p0010010 integer,   -- Population of two races:
    p0010011 integer,       -- White; Black or African American
    p0010012 integer,       -- White; American Indian and Alaska Native
    p0010013 integer,       -- White; Asian
    p0010014 integer,       -- White; Native Hawaiian and Other Pacific Islander
    p0010015 integer,       -- White; Some Other Race
    p0010016 integer,       -- Black or African American; American Indian and Alaska Native
    p0010017 integer,       -- Black or African American; Asian
    p0010018 integer,       -- Black or African American; Native Hawaiian and Other Pacific Islander
    p0010019 integer,       -- Black or African American; Some Other Race
    p0010020 integer,       -- American Indian and Alaska Native; Asian
    p0010021 integer,       -- American Indian and Alaska Native; Native Hawaiian and Other Pacific Islander
    p0010022 integer,       -- American Indian and Alaska Native; Some Other Race
    p0010023 integer,       -- Asian; Native Hawaiian and Other Pacific Islander
    p0010024 integer,       -- Asian; Some Other Race
    p0010025 integer,       -- Native Hawaiian and Other Pacific Islander; Some Other Race
    p0010026 integer,   -- Population of three races
    p0010047 integer,   -- Population of four races
    p0010063 integer,   -- Population of five races
    p0010070 integer,   -- Population of six races

    -- This section is referred to as P2. HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
    p0020001 integer,   -- Total
    p0020002 integer,   -- Hispanic or Latino
    p0020003 integer,   -- Not Hispanic or Latino:
    p0020004 integer,   -- Population of one race:
    p0020005 integer,       -- White Alone
    p0020006 integer,       -- Black or African American alone
    p0020007 integer,       -- American Indian and Alaska Native alone
    p0020008 integer,       -- Asian alone
    p0020009 integer,       -- Native Hawaiian and Other Pacific Islander alone
    p0020010 integer,       -- Some Other Race alone
    p0020011 integer,   -- Two or More Races
    p0020012 integer,   -- Population of two races
    p0020028 integer,   -- Population of three races
    p0020049 integer,   -- Population of four races
    p0020065 integer,   -- Population of five races
    p0020072 integer,   -- Population of six races

    -- This section is referred to as P3. RACE FOR THE POPULATION 18 YEARS AND OVER
    p0030001 integer,   -- Total
    p0030002 integer,   -- Population of one race:
    p0030003 integer,       -- White alone
    p0030004 integer,       -- Black or African American alone
    p0030005 integer,       -- American Indian and Alaska Native alone
    p0030006 integer,       -- Asian alone
    p0030007 integer,       -- Native Hawaiian and Other Pacific Islander alone
    p0030008 integer,       -- Some Other Race alone
    p0030009 integer,   -- Two or More Races
    p0030010 integer,   -- Population of two races
    p0030026 integer,   -- Population of three races
    p0030047 integer,   -- Population of four races
    p0030063 integer,   -- Population of five races
    p0030070 integer,   -- Population of six races

    -- This section is referred to as P4. HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
    -- FOR THE POPULATION 18 YEARS AND OVER
    p0040001 integer,   -- Total
    p0040002 integer,   -- Hispanic or Latino
    p0040003 integer,   -- Not Hispanic or Latino:
    p0040004 integer,   -- Population of one race:
    p0040005 integer,   -- White alone
    p0040006 integer,   -- Black or African American alone
    p0040007 integer,   -- American Indian and Alaska Native alone
    p0040008 integer,   -- Asian alone
    p0040009 integer,   -- Native Hawaiian and Other Pacific Islander alone
    p0040010 integer,   -- Some Other Race alone
    p0040011 integer,   -- Two or More Races
    p0040012 integer,   -- Population of two races
    p0040028 integer,   -- Population of three races
    p0040049 integer,   -- Population of four races
    p0040065 integer,   -- Population of five races
    p0040072 integer,   -- Population of six races

    -- This section is referred to as H1. OCCUPANCY STATUS
    h0010001 integer,   -- Total housing units
    h0010002 integer,   -- Occupied
    h0010003 integer    -- Vacant
);

COPY us_counties_2010
FROM '/tmp/us_counties_2010.csv'
WITH (FORMAT CSV, HEADER);

-- Calculate on multiple columns from two tables with JOIN and multiple ON conditions
SELECT c2010.geo_name,
	c2010.state_us_abbreviation AS state,
	c2010.p0010001 AS pop_2010,
	c2000.p0010001 AS pop_2000,
	c2010.p0010001 - c2000.p0010001 AS raw_change,
	round( (CAST(c2010.p0010001 AS numeric(8, 1)) - c2000.p0010001) / c2000.p0010001 * 100, 1) AS pct_change
FROM us_counties_2010 AS c2010 INNER JOIN us_counties_2000 AS c2000
ON c2010.state_fips = c2000.state_fips
	AND c2010.county_fips = c2000.county_fips
	AND c2010.p0010001 <> c2000.p0010001
ORDER BY pct_change DESC;
