-- Extract components of a timestamp value using date_part
SELECT
    date_part('year', '2019-12-01 18:37:12 EST'::timestamptz) AS "year",
    date_part('month', '2019-12-01 18:37:12 EST'::timestamptz) AS "month",
    date_part('day', '2019-12-01 18:37:12 EST'::timestamptz) AS "day",
    date_part('hour', '2019-12-01 18:37:12 EST'::timestamptz) AS "hour",
    date_part('minute', '2019-12-01 18:37:12 EST'::timestamptz) AS "minute",
    date_part('seconds', '2019-12-01 18:37:12 EST'::timestamptz) AS "seconds",
    date_part('timezone_hour', '2019-12-01 18:37:12 EST'::timestamptz) AS "tz",
    date_part('week', '2019-12-01 18:37:12 EST'::timestamptz) AS "week",
    date_part('quarter', '2019-12-01 18:37:12 EST'::timestamptz) AS "quarter",
    date_part('epoch', '2019-12-01 18:37:12 EST'::timestamptz) AS "epoch";

SELECT extract('year' from '2019-12-01 18:37:12 EST'::timestamptz) AS "year";

-- Make timestamp from components
SELECT make_date(2018, 2, 22);
SELECT make_time(18, 4, 30.3);
SELECT make_timestamptz(2018, 2, 22, 18, 4, 30.3, 'Europe/Lisbon');

-- SELECT current_date, current_time, current_timestamp, localtime, localtimestamp, now();
SELECT now();
SELECT current_time;

-- Comparing current_timestamp and clock_timestamp()
DROP TABLE IF EXISTS current_time_example;
CREATE TABLE IF NOT EXISTS current_time_example (
	time_id bigserial,
	current_timestamp_col timestamp with time zone,
	clock_timestamp_col timestamp with time zone
);

INSERT INTO current_time_example (current_timestamp_col, clock_timestamp_col)
	(SELECT current_timestamp, clock_timestamp()
		FROM generate_series(1, 1000));

SELECT * FROM current_time_example
LIMIT 10;

-- Work with time zones

SHOW timezone; -- Show my PostgreSQL server's default time zone
SELECT * FROM pg_timezone_names
WHERE name LIKE '%Berlin%' OR name LIKE '%Shanghai%';

SET timezone TO 'Europe/Berlin';

DROP TABLE IF EXISTS time_zone_test;
CREATE TABLE IF NOT EXISTS time_zone_test(
	test_date timestamp with time zone
);
INSERT INTO time_zone_test VALUES ('2022-07-08 21:00');
SELECT test_date FROM time_zone_test;

SET timezone TO 'Asia/Shanghai';
SELECT test_date FROM time_zone_test;

SELECT '1929-9-30'::date - '1929-9-27'::date;
SELECT '1929-9-30'::date + '5 years'::interval;

-- Examples with the taxi data

DROP TABLE IF EXISTS nyc_yellow_taxi_trips_2016_06_01;
CREATE TABLE IF NOT EXISTS nyc_yellow_taxi_trips_2016_06_01 (
    trip_id bigserial PRIMARY KEY,
    vendor_id varchar(1) NOT NULL,
    tpep_pickup_datetime timestamp with time zone NOT NULL,
    tpep_dropoff_datetime timestamp with time zone NOT NULL,
    passenger_count integer NOT NULL,
    trip_distance numeric(8,2) NOT NULL,
    pickup_longitude numeric(18,15) NOT NULL,
    pickup_latitude numeric(18,15) NOT NULL,
    rate_code_id varchar(2) NOT NULL,
    store_and_fwd_flag varchar(1) NOT NULL,
    dropoff_longitude numeric(18,15) NOT NULL,
    dropoff_latitude numeric(18,15) NOT NULL,
    payment_type varchar(1) NOT NULL,
    fare_amount numeric(9,2) NOT NULL,
    extra numeric(9,2) NOT NULL,
    mta_tax numeric(5,2) NOT NULL,
    tip_amount numeric(9,2) NOT NULL,
    tolls_amount numeric(9,2) NOT NULL,
    improvement_surcharge numeric(9,2) NOT NULL,
    total_amount numeric(9,2) NOT NULL
);

COPY nyc_yellow_taxi_trips_2016_06_01 (
    vendor_id,
    tpep_pickup_datetime,
    tpep_dropoff_datetime,
    passenger_count,
    trip_distance,
    pickup_longitude,
    pickup_latitude,
    rate_code_id,
    store_and_fwd_flag,
    dropoff_longitude,
    dropoff_latitude,
    payment_type,
    fare_amount,
    extra,
    mta_tax,
    tip_amount,
    tolls_amount,
    improvement_surcharge,
    total_amount
   )
FROM '/tmp/yellow_tripdata_2016_06_01.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

CREATE INDEX tpep_pickup_idx
ON nyc_yellow_taxi_trips_2016_06_01 (tpep_pickup_datetime);

-- Count taxi trips by hour
SELECT
	date_part('hour', tpep_pickup_datetime) AS trip_hour,
	count(*) -- Aggregate all other columns
FROM nyc_yellow_taxi_trips_2016_06_01
GROUP BY trip_hour
ORDER BY trip_hour;

COPY
	(SELECT 
		date_part('hour', tpep_pickup_datetime) AS trip_hour,
		count(*)
	 FROM nyc_yellow_taxi_trips_2016_06_01
	 GROUP BY trip_hour
	 ORDER BY trip_hour
	)
TO '/tmp/hourly_pickups_2016_06_01.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

-- Calculate the median trip time by hour
SELECT
	date_part('hour', tpep_pickup_datetime) AS trip_hour,
	percentile_cont(.5)
		WITHIN GROUP (ORDER BY tpep_dropoff_datetime - tpep_pickup_datetime) AS median_trip
FROM nyc_yellow_taxi_trips_2016_06_01
GROUP BY trip_hour
ORDER BY median_trip DESC;


DROP TABLE nyc_yellow_taxi_trips_2016_06_01;
DROP TABLE time_zone_test;
DROP TABLE  current_time_example;
