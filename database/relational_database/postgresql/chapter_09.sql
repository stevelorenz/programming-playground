DROP TABLE IF EXISTS meat_poultry_egg_inspect;
CREATE TABLE IF NOT EXISTS meat_poultry_egg_inspect (
    est_number varchar(50) CONSTRAINT est_number_key PRIMARY KEY,
    company varchar(100),
    street varchar(100),
    city varchar(30),
    st varchar(2),
    zip varchar(5),
    phone varchar(14),
    grant_date date,
    activities text,
    dbas text
);

COPY meat_poultry_egg_inspect
FROM '/tmp/MPI_Directory_by_Establishment_Name.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

-- Create index on company
CREATE INDEX company_idx ON meat_poultry_egg_inspect (company);

SELECT count(*) FROM meat_poultry_egg_inspect;

-- Try to find companies that have the same address
SELECT company, street, city, st, count(*) AS address_count
FROM meat_poultry_egg_inspect
GROUP BY company, street, city, st
HAVING count(*) > 1
ORDER BY company, street, city, st;

-- Check missing values in the st column
SELECT st, count(*) AS st_count
FROM meat_poultry_egg_inspect
GROUP BY st
ORDER BY st
LIMIT 10;

SELECT est_number, company, city, st, zip
FROM meat_poultry_egg_inspect
WHERE st is NULL;

-- SELECT company, count(*) AS company_count
-- FROM meat_poultry_egg_inspect
-- GROUP BY company
-- ORDER BY company ASC
-- LIMIT 50;

SELECT length(zip), count(*) AS length_count
FROM meat_poultry_egg_inspect
GROUP BY length(zip)
ORDER BY length(zip) ASC;

SELECT st, count(st) as st_count
FROM meat_poultry_egg_inspect
WHERE length(zip) < 5 -- the correct length of zip should be 5
GROUP BY st
ORDER BY st ASC;

-- Backup the orignal table before updating
CREATE TABLE meat_poultry_egg_inspect_backup AS
SELECT * FROM meat_poultry_egg_inspect;

ALTER TABLE meat_poultry_egg_inspect ADD st_copy varchar(2);

UPDATE meat_poultry_egg_inspect
SET st_copy = st;

-- Update the missing st values for three establishements
UPDATE meat_poultry_egg_inspect
SET st = 'MN'
WHERE est_number = 'V18677A';

UPDATE meat_poultry_egg_inspect
SET st = 'AL'
WHERE est_number = 'M45319+P45319';

UPDATE meat_poultry_egg_inspect
SET st = 'WI'
WHERE est_number = 'M263A+P263A+V263A';

-- Restore from the backup column
UPDATE meat_poultry_egg_inspect
SET st = st_copy;

-- Fix company names
ALTER TABLE meat_poultry_egg_inspect ADD COLUMN company_standard varchar(100);
UPDATE meat_poultry_egg_inspect
SET company_standard = 'Armour-Eckrich Meats'
WHERE company LIKE 'Armour%';

SELECT company, company_standard
FROM meat_poultry_egg_inspect
WHERE company LIKE 'Armour%';

-- Update zip code with string concatenation

ALTER TABLE meat_poultry_egg_inspect ADD COLUMN zip_copy varchar(5);
UPDATE meat_poultry_egg_inspect
SET zip_copy = zip;

UPDATE meat_poultry_egg_inspect
SET zip = '00' || zip
WHERE st IN('PR', 'VI') AND length(zip) = 3;

UPDATE meat_poultry_egg_inspect
SET zip = '0' || zip
WHERE st IN('CT', 'MA', 'ME', 'NH', 'NJ', 'RI', 'VT') AND length(zip) = 4;

SELECT st, zip
FROM meat_poultry_egg_inspect
WHERE st IN('PR', 'CT', 'MA')
GROUP BY st, zip;

SELECT count(*)
FROM meat_poultry_egg_inspect
WHERE length(zip) <> 5; -- Should return 0

-- Create the state_regions table
DROP TABLE IF EXISTS state_regions;
CREATE TABLE IF NOT EXISTS state_regions (
	st varchar(2) CONSTRAINT st_key PRIMARY KEY,
	region varchar(20) NOT NULL
);

COPY state_regions
FROM '/tmp/state_regions.csv'
WITH (FORMAT CSV, HEADER, DELIMITER ',');

ALTER TABLE meat_poultry_egg_inspect ADD COLUMN inspect_date date;

UPDATE meat_poultry_egg_inspect AS inspect
SET inspect_date = '2019-12-01'
-- WHERE EXISTS clause is used to check the existence of column/rows in an another table.
WHERE EXISTS (SELECT state_regions.region
			  FROM state_regions
			  WHERE inspect.st = state_regions.st
			  	AND state_regions.region = 'New England');

SELECT st, inspect_date
FROM meat_poultry_egg_inspect
GROUP BY st, inspect_date
ORDER BY st;

-- Delete rows matching an expression
DELETE FROM meat_poultry_egg_inspect
WHERE st IN('PR', 'VI');

ALTER TABLE meat_poultry_egg_inspect DROP COLUMN zip_copy;

DROP TABLE meat_poultry_egg_inspect_backup;

-- A demo for transaction block
START TRANSACTION;

UPDATE meat_poultry_egg_inspect
SET company = 'AGRO Merchantss Oakland LLC'
WHERE company = 'AGRO Merchants Oakland LLC';

SELECT company
FROM meat_poultry_egg_inspect
WHERE company LIKE 'AGRO%'
ORDER BY company;

-- Revert the transaction without actually commit it.
ROLLBACK;

-- Commit changes to the datebase
START TRANSACTION;
UPDATE meat_poultry_egg_inspect
SET company = 'AGRO Merchants Oakland LLC'
WHERE company = 'AGRO Merchants Oakland, LLC';
COMMIT;

-- Backing up a table while adding or filling a new column
CREATE TABLE meat_poultry_egg_inspect_backup AS
SELECT *, '2018-02-07'::date AS reviewed_date
FROM meat_poultry_egg_inspect;

DROP TABLE state_regions;
DROP TABLE meat_poultry_egg_inspect;
