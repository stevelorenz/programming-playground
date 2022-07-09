DROP TABLE IF EXISTS acs_2011_2015_stats;
CREATE TABLE IF NOT EXISTS acs_2011_2015_stats (
    geoid varchar(14) CONSTRAINT geoid_key PRIMARY KEY,
    county varchar(50) NOT NULL,
    st varchar(20) NOT NULL,
    pct_travel_60_min numeric(5,3) NOT NULL,
    pct_bachelors_higher numeric(5,3) NOT NULL,
    pct_masters_higher numeric(5,3) NOT NULL,
    median_hh_income integer,
    CHECK (pct_masters_higher <= pct_bachelors_higher)
);

COPY acs_2011_2015_stats
FROM '/tmp/acs_2011_2015_stats.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

-- Use corr(Y, X) to measure relationship between education and salary
select corr(median_hh_income, pct_bachelors_higher)
	as bachelors_income_r
from acs_2011_2015_stats;

select corr(median_hh_income, pct_masters_higher)
	as masters_income_r
from acs_2011_2015_stats;

-- Use linear regression
SELECT 
	round(regr_slope(median_hh_income, pct_bachelors_higher)::numeric, 2) AS slope,
	round(regr_intercept(median_hh_income, pct_bachelors_higher)::numeric, 2) AS y_intercept
FROM acs_2011_2015_stats;

-- Demo of using rank functions

DROP TABLE IF EXISTS widget_output;
CREATE TABLE IF NOT EXISTS widget_companies (
    id bigserial,
    company varchar(30) NOT NULL,
    widget_output integer NOT NULL
);

INSERT INTO widget_companies (company, widget_output)
VALUES
    ('Morse Widgets', 125000),
    ('Springfield Widget Masters', 143000),
    ('Best Widgets', 196000),
    ('Acme Inc.', 133000),
    ('District Widget Inc.', 201000),
    ('Clarke Amalgamated', 620000),
    ('Stavesacre Industries', 244000),
    ('Bowers Widget Emporium', 201000);

SELECT company, widget_output,
	rank() OVER (ORDER BY widget_output DESC),
	dense_rank() OVER (ORDER BY widget_output DESC)
FROM widget_companies;

-- Use rank within groups using PARTITION

DROP TABLE IF EXISTS store_sales;
CREATE TABLE IF NOT EXISTS store_sales (
    store varchar(30),
    category varchar(30) NOT NULL,
    unit_sales bigint NOT NULL,
    CONSTRAINT store_category_key PRIMARY KEY (store, category)
);

INSERT INTO store_sales (store, category, unit_sales)
VALUES
    ('Broders', 'Cereal', 1104),
    ('Wallace', 'Ice Cream', 1863),
    ('Broders', 'Ice Cream', 2517),
    ('Cramers', 'Ice Cream', 2112),
    ('Broders', 'Beer', 641),
    ('Cramers', 'Cereal', 1003),
    ('Cramers', 'Beer', 640),
    ('Wallace', 'Cereal', 980),
    ('Wallace', 'Beer', 988);

SELECT
	category,
	store,
	unit_sales,
	-- Partion by will create ranks for each individual category, instead of all rows!
	rank() OVER (PARTITION BY category ORDER BY unit_sales DESC)
FROM store_sales;

DROP TABLE IF EXISTS fbi_crime_data_2015;
CREATE TABLE IF NOT EXISTS fbi_crime_data_2015 (
    st varchar(20),
    city varchar(50),
    population integer,
    violent_crime integer,
    property_crime integer,
    burglary integer,
    larceny_theft integer,
    motor_vehicle_theft integer,
    CONSTRAINT st_city_key PRIMARY KEY (st, city)
);

COPY fbi_crime_data_2015
FROM '/tmp/fbi_crime_data_2015.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

-- Calculate the crime rates per thousand in cities with more than 500,000 people
SELECT
	city,
	st,
	population,
	property_crime,
	round((property_crime::numeric / population) * 1000 ,1) AS pct_per_1000
FROM fbi_crime_data_2015
WHERE population >= 500000
ORDER BY (property_crime::numeric / population) DESC;


DROP TABLE fbi_crime_data_2015;
DROP TABLE store_sales;
DROP TABLE acs_2011_2015_stats;
DROP TABLE widget_companies;
