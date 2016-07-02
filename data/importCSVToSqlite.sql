--
-- cat gtfsbrisbane/data/importCSVToSqlite.sql | sqlite3 -interactive -csv gtfs.sqlite
--
.mode csv

BEGIN TRANSACTION;


DROP TABLE IF EXISTS temp;
DROP TABLE IF EXISTS trip;
DROP TABLE IF EXISTS stop_time;
DROP TABLE IF EXISTS stop;
DROP TABLE IF EXISTS calendar;
DROP TABLE IF EXISTS route;



-- routes
--
.import routes.txt temp

CREATE TABLE "route" (
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"route_id" VARCHAR NOT NULL,
"short_name" VARCHAR NOT NULL,
"long_name" VARCHAR NOT NULL,
"desc" VARCHAR NULL,
"type" VARCHAR NOT NULL,
"url" VARCHAR NULL,
"color" VARCHAR NULL,
"text_color" VARCHAR NULL
);

insert into route (route_id, short_name, long_name, desc, type, url, color, text_color)
select route_id, route_short_name, route_long_name, route_desc, route_type, route_url, route_color, route_text_color
from temp;

-- trips
--
DROP TABLE temp;
.import trips.txt temp

CREATE TABLE "trip"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"trip_id" VARCHAR NOT NULL,
"route_id" INTEGER NOT NULL REFERENCES "route",
"service_id" VARCHAR NOT NULL,
"headsign" VARCHAR NULL,
"short_name" VARCHAR NULL,
"direction_id" BOOLEAN NULL,
"block_id" VARCHAR NULL,
"shape_id" VARCHAR NULL,
"wheelchair_accessible" INTEGER NULL,
"bikes_allowed" INTEGER NULL
);
insert into trip (trip_id, route_id, service_id, headsign, short_name, direction_id, block_id, shape_id, wheelchair_accessible, bikes_allowed)
select trip_id, route.id, service_id, trip_headsign, NULL, direction_id, block_id, shape_id, NULL, NULL
from temp, route
where temp.route_id = route.route_id;

-- stops
--
DROP TABLE temp;
.import stops.txt temp

CREATE TABLE "stop"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"stop_id" VARCHAR NOT NULL,
"code" VARCHAR NULL,
"name" VARCHAR NOT NULL,
"desc" VARCHAR NULL,
"lat" REAL NOT NULL,
"lon" REAL NOT NULL,
"zone_id" VARCHAR NULL,
"url" VARCHAR NULL,
"location_type" INTEGER NULL,
"parent_station" VARCHAR NULL);

INSERT INTO stop (stop_id, code, name, desc, lat, lon, zone_id, url, location_type, parent_station)
select stop_id, stop_code, stop_name, stop_desc, stop_lat, stop_lon, zone_id, stop_url, location_type, parent_station
from temp;

-- stop_times
--
DROP TABLE temp;
.import stop_times.txt temp

CREATE TABLE "stop_time"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"trip_id" INTEGER NOT NULL REFERENCES "trip",
"trip" VARCHAR NOT NULL,
"arrival_time" TIME NOT NULL,
"departure_time" TIME NOT NULL,
"stop" VARCHAR NOT NULL,
"stop_id" INTEGER NOT NULL REFERENCES "stop",
"stop_sequence" VARCHAR NOT NULL,
"pickup_type" INTEGER NULL,
"drop_off_type" INTEGER NULL
);

-- XXX the time function here obviously can't convert 25:0X:XX entries in the
-- CSV ... not sure what I do about them... perhaps skip them as invalid for now?
--
insert into stop_time (trip_id, trip, arrival_time, departure_time, stop, stop_id, stop_sequence, pickup_type, drop_off_type)
select trip.id, temp.trip_id, time(temp.arrival_time), time(departure_time), temp.stop_id, stop.id, stop_sequence, pickup_type, drop_off_type
from temp, trip, stop
where temp.trip_id = trip.trip_id and temp.stop_id = stop.stop_id;

CREATE INDEX stop_seq_index ON stop_time (trip_id, stop_sequence);

-- calendar
--
DROP TABLE temp;
.import calendar.txt temp

CREATE TABLE "calendar"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"service_id" VARCHAR NOT NULL,
"monday" VARCHAR NOT NULL,
"tuesday" VARCHAR NOT NULL,
"wednesday" VARCHAR NOT NULL,
"thursday" VARCHAR NOT NULL,
"friday" VARCHAR NOT NULL,
"saturday" VARCHAR NOT NULL,
"sunday" VARCHAR NOT NULL,
"start_date" DATE NOT NULL,
"end_date" DATE NOT NULL
);

insert into calendar (service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday, start_date, end_date)
select service_id, monday, tuesday, wednesday, thursday, friday, saturday, sunday,
       (substr(start_date, 1, 4) || '-' || substr(start_date, 5, 2) || '-' || substr(start_date, 7)),
       (substr(end_date, 1, 4) || '-' || substr(end_date, 5, 2) || '-' || substr(end_date, 7))
from temp;

DROP TABLE IF EXISTS temp;
COMMIT;
