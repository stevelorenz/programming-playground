-- Some common used string functions
SELECT upper('Neal7');
SELECT initcap('Practical SQL');

SELECT char_length(' Pat ');
SELECT length(' Pat ');
SELECT position(', ' in 'Tan, Bella');

-- Remove characters
SELECT trim('s' from 'socks');
SELECT trim(trailing 's' from 'socks');
SELECT trim(' Pat ');
SELECT ltrim('socks', 's');
SELECT rtrim('socks', 's');

-- Replace characters
SELECT left('703-555-1212', 3);
SELECT right('703-555-1212', 8);
SELECT replace('bat', 'b', 'c');

-- Examples for regular expression
--- Any characters one or more times
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from '.+');
--- One or two digits followed by a space and p.m.
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from '\d{1,2} (?:a.m|p.m)');
--- One or more word characters at the start
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from '^\w+');
--- The words May or June
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from 'May|June');
--- Four digits
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from '\d{4}');
--- May followed by a space, digit, comma, and four digits
SELECT substring('The game starts at 7 p.m. on May 2, 2019.' from 'May \d, \d{4}');

-- Turning Text to Data with Regular Expression Functions

DROP TABLE IF EXISTS crime_reports;
CREATE TABLE IF NOT EXISTS crime_reports (
    crime_id bigserial PRIMARY KEY,
    date_1 timestamp with time zone,
    date_2 timestamp with time zone,
    street varchar(250),
    city varchar(100),
    crime_type varchar(100),
    description text,
    case_number varchar(50),
    original_text text NOT NULL
);

COPY crime_reports (original_text)
FROM '/tmp/crime_reports.csv'
WITH (FORMAT CSV, HEADER OFF, QUOTE '"');

SELECT original_text FROM crime_reports
LIMIT 1;

--- Find the first date
SELECT crime_id,
	regexp_match(original_text, '\d{1,2}\/\d{1,2}\/\d{2}')
FROM crime_reports;

SELECT crime_id,
	regexp_matches(original_text, '\d{1,2}\/\d{1,2}\/\d{2}', 'g')
FROM crime_reports;

SELECT crime_id,
	regexp_matches(original_text, '-(\d{1,2}\/\d{1,2}\/\d{2})')
FROM crime_reports;

SELECT
	regexp_match(original_text, '(?:C0|S0)[0-9]+') AS case_number,
	regexp_match(original_text, '\n(?:\w+ \w|\w+)\n(.*):') AS crime_type
FROM crime_reports;

-- Use regrex to parse components and get them at once
-- SELECT crime_id,
--        regexp_match(original_text, '\d{1,2}\/\d{1,2}\/\d{2}') AS date_1,
--        CASE WHEN EXISTS (SELECT regexp_matches(original_text, '-(\d{1,2}\/\d{1,2}\/\d{1,2})'))
--             THEN regexp_match(original_text, '-(\d{1,2}\/\d{1,2}\/\d{1,2})')
--             ELSE NULL
--             END AS date_2,
--        regexp_match(original_text, '\/\d{2}\n(\d{4})') AS hour_1,
--        CASE WHEN EXISTS (SELECT regexp_matches(original_text, '\/\d{2}\n\d{4}-(\d{4})'))
--             THEN regexp_match(original_text, '\/\d{2}\n\d{4}-(\d{4})')
--             ELSE NULL
--             END AS hour_2,
--        regexp_match(original_text, 'hrs.\n(\d+ .+(?:Sq.|Plz.|Dr.|Ter.|Rd.))') AS street,
--        regexp_match(original_text, '(?:Sq.|Plz.|Dr.|Ter.|Rd.)\n(\w+ \w+|\w+)\n') AS city,
--        regexp_match(original_text, '\n(?:\w+ \w+|\w+)\n(.*):') AS crime_type,
--        regexp_match(original_text, ':\s(.+)(?:C0|SO)') AS description,
--        regexp_match(original_text, '(?:C0|SO)[0-9]+') AS case_number
-- FROM crime_reports
-- LIMIT 3;

-- Full text search
SELECT to_tsvector('I am walking across the sitting room to sit with you.');
SELECT to_tsquery('walking & sitting');

SELECT to_tsvector('I am walking across the sitting room') @@ to_tsquery('walking & sitting');
SELECT to_tsvector('I am walking across the sitting room') @@ to_tsquery('walking & running');

-- Sources:
-- https://archive.org/details/State-of-the-Union-Addresses-1945-2006
-- http://www.presidency.ucsb.edu/ws/index.php
-- https://www.eisenhower.archives.gov/all_about_ike/speeches.html

DROP TABLE IF EXISTS president_speeches;
CREATE TABLE IF NOT EXISTS president_speeches (
    sotu_id serial PRIMARY KEY,
    president varchar(100) NOT NULL,
    title varchar(250) NOT NULL,
    speech_date date NOT NULL,
    speech_text text NOT NULL,
    search_speech_text tsvector
);

COPY president_speeches (president, title, speech_date, speech_text)
FROM '/tmp/sotu-1946-1977.csv'
WITH (FORMAT CSV, DELIMITER '|', HEADER OFF, QUOTE '@');

-- Convert speeches to tsvector in the search_speech_text column
UPDATE president_speeches
SET search_speech_text = to_tsvector('english', speech_text);

CREATE INDEX search_idx ON president_speeches USING gin(search_speech_text);

SELECT president, speech_date
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('Vietnam')
ORDER BY speech_date;

SELECT president, speech_date
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('China')
ORDER BY speech_date;

-- Show search results with ts_headline
SELECT president,
	speech_date,
	ts_headline(speech_text, to_tsquery('China'),
		'StartSel = <,
        StopSel = >,
        MinWords=5,
        MaxWords=7,
        MaxFragments=1')
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('China');

-- Find adjacent words
SELECT president,
	speech_date,
	ts_headline(speech_text, to_tsquery('military <-> defense'),
		'StartSel = <,
        StopSel = >,
        MinWords=5,
        MaxWords=7,
        MaxFragments=1')
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('military <-> defense');

-- Instead of adjacent, show an example of distance 2
SELECT president,
	speech_date,
	ts_headline(speech_text, to_tsquery('military <2> defense'),
		'StartSel = <,
        StopSel = >,
        MinWords=5,
        MaxWords=7,
        MaxFragments=1')
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('military <2> defense');

SELECT president, speech_date,
	ts_rank(search_speech_text, to_tsquery('war & security & threat & enemy')) AS score
FROM president_speeches
WHERE search_speech_text @@ to_tsquery('war & security & threat & enemy')
ORDER BY score DESC
LIMIT 5;

DROP TABLE president_speeches;
DROP TABLE crime_reports;
