.mode csv

BEGIN TRANSACTION;

.import trips.txt temp

CREATE TABLE "trip"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"trip_id" VARCHAR NOT NULL,
"route_id" VARCHAR NOT NULL,
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
select trip_id, route_id, service_id, trip_headsign, NULL, direction_id, block_id, shape_id, NULL, NULL
from temp;

drop table temp;

-- stop_times
--
.import stop_times.txt temp

CREATE TABLE "stop_time"(
"id" INTEGER PRIMARY KEY AUTOINCREMENT,
"trip" INTEGER NOT NULL REFERENCES "trip",
"trip_id" VARCHAR NOT NULL,
"arrival_time" TIME NOT NULL,
"departure_time" TIME NOT NULL,
"stop" VARCHAR NOT NULL,
"stop_sequence" VARCHAR NOT NULL,
"pickup_type" INTEGER NULL,
"drop_off_type" INTEGER NULL
);

-- XXX the time function here obviously can't convert 25:0X:XX entries in the
-- CSV ... not sure what I do about them... perhaps skip them as invalid for now?
--
insert into stop_time (trip, trip_id, arrival_time, departure_time, stop, stop_sequence, pickup_type, drop_off_type)
select trip.id, temp.trip_id, time(temp.arrival_time), time(departure_time), stop_id, stop_sequence, pickup_type, drop_off_type
from temp, trip
where temp.trip_id = trip.trip_id;

.import calendar.txt calendar

COMMIT;
