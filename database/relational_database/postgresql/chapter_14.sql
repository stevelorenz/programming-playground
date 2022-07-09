CREATE EXTENSION IF NOT EXISTS postgis;

-- SELECT postgis_full_version();

-- SELECT srtext
-- FROM spatial_ref_sys
-- WHERE srid = 4326;

-- Create spatial objects
SELECT ST_GeomFromText('POINT(-74.9233606 42.699992)', 4326);
SELECT ST_GeomFromText('LINESTRING(-74.9 42.7, -75.1 42.7)', 4326);

-- SELECT
-- ST_GeogFromText('SRID=4326;MULTIPOINT(-74.9 42.7, -75.1 42.7, -74.924 42.6)');

DROP TABLE IF EXISTS farmers_markets;
CREATE TABLE IF NOT EXISTS farmers_markets (
    fmid bigint PRIMARY KEY,
    market_name varchar(100) NOT NULL,
    street varchar(180),
    city varchar(60),
    county varchar(25),
    st varchar(20) NOT NULL,
    zip varchar(10),
    longitude numeric(10,7),
    latitude numeric(10,7),
    organic varchar(1) NOT NULL
);

COPY farmers_markets
FROM '/tmp/farmers_markets.csv'
WITH (FORMAT CSV, HEADER);

-- Create and index a geography column
ALTER TABLE farmers_markets ADD COLUMN geog_point geography(POINT, 4326);

-- Fill geography data with latitude and longitude
UPDATE farmers_markets
SET geog_point = ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)::geography;

CREATE INDEX market_pts_idx ON farmers_markets USING GIST (geog_point);

SELECT longitude, latitude,
	geog_point, ST_AsText(geog_point)
FROM farmers_markets
WHERE longitude IS NOT NULL
LIMIT 5;

-- Locate farmer's market within 10 km of a POINT
SELECT market_name, city, st
FROM farmers_markets
WHERE ST_DWithin(geog_point, ST_GeogFromText('POINT(-93.6204386 41.5853202)'), 1000)
ORDER BY market_name;

-- Use Distance() to calculate the miles between Yankee Stadium and Citi Field
SELECT ST_Distance(
	ST_GeogFromText('POINT(-73.9283685 40.8296466)'),
	ST_GeogFromText('POINT(-73.8480153 40.7570917)')
);

SELECT market_name,
       city,
       round(
           (ST_Distance(geog_point,
                        ST_GeogFromText('POINT(-93.6204386 41.5853202)')
                        ))::numeric(8,1), 2
            ) AS miles_from_dt
FROM farmers_markets
WHERE ST_DWithin(geog_point,
                 ST_GeogFromText('POINT(-93.6204386 41.5853202)'),
                 10000)
ORDER BY miles_from_dt ASC;


DROP TABLE farmers_markets;
