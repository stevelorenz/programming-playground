DROP TABLE IF EXISTS char_data_types;

CREATE TABLE IF NOT EXISTS char_data_types (
	varchar_column varchar(10),
	char_column char(10),
	text_column text
);

INSERT INTO char_data_types
VALUES
	('abc', 'abc', 'abc'),
	('defghi', 'defghi', 'defghi');

SELECT *
FROM  char_data_types;

COPY char_data_types TO '/tmp/typetest.txt'
WITH (FORMAT CSV, HEADER, DELIMITER '|');


DROP TABLE IF EXISTS number_data_types;

CREATE TABLE IF NOT EXISTS number_data_types (
	numeric_column numeric(20, 5),
	real_column real,
	double_column double precision
);

INSERT INTO number_data_types
VALUES
	(0.7, 0.7, 0.7),
	(2.13579, 2.13579, 2.13579),
	(2.1357987654, 2.1357987654, 2.1357987654);

SELECT * FROM number_data_types;

-- So the float point number is not really 0.7, it's something like 0.69999999999
SELECT
	numeric_column * 10000000 AS "Fixed",
	real_column * 10000000 AS "Float"
FROM number_data_types
WHERE numeric_column = 0.7;

DROP TABLE IF EXISTS date_time_types;

CREATE TABLE IF NOT EXISTS date_time_types(
	timestamp_column timestamp with time zone,
	interval_column interval
);

INSERT INTO date_time_types
VALUES
	('2018-12-31 01:00 EST', '2 days'),
	('2018-12-31 01:00 PST', '1 month'),
	('2018-12-31 01:00 Australia/Melbourne', '1 century'),
	(now(), '1 week');

SELECT *
FROM date_time_types;

-- Using interval type for calculation
SELECT timestamp_column, interval_column,
	timestamp_column - interval_column AS new_date
FROM date_time_types;

-- Use CAST to convert types
SELECT timestamp_column, CAST(timestamp_column AS varchar(10))
FROM date_time_types;

SELECT numeric_column,
	CAST(numeric_column AS integer),
	CAST(numeric_column AS varchar(6))
FROM number_data_types;

-- :: is the alternative notation of CAST()
SELECT timestamp_column::varchar(10)
FROM date_time_types;
