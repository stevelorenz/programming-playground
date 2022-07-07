SELECT 11 / 6;
SELECT 11 % 6;
SELECT 11.0 / 6; -- decimal division
SELECT CAST(11 AS numeric(3, 1)) / 6;

SELECT 3 ^ 4;
SELECT sqrt(10);
SELECT ||/ 10; --- cube root
SELECT 4 !;

DROP TABLE IF EXISTS percent_change;
CREATE TABLE IF NOT EXISTS percent_change (
    department varchar(20),
    spend_2014 numeric(10,2),
    spend_2017 numeric(10,2)
);

INSERT INTO percent_change
VALUES
    ('Building', 250000, 289000),
    ('Assessor', 178556, 179500),
    ('Library', 87777, 90001),
    ('Clerk', 451980, 650000),
    ('Police', 250000, 223000),
    ('Recreation', 199000, 195000);

SELECT department, spend_2014, spend_2017,
	round((spend_2017 - spend_2014) / spend_2014 * 100, 2) AS "pct_change"
FROM percent_change;

DROP TABLE IF EXISTS percentile_test;
CREATE TABLE IF NOT EXISTS percentile_test (
	numbers integer
);

INSERT INTO percentile_test (numbers)
VALUES
	(1), (2), (3), (4), (5), (6);

SELECT * FROM percentile_test;

-- Calculate percentile (for e.g. median)
SELECT
	percentile_cont(.5)
	WITHIN GROUP (ORDER BY numbers), -- returns a value corresponding to the specified fraction in the ordering
	percentile_disc(.5)
	WITHIN GROUP (ORDER BY numbers)
FROM percentile_test;
