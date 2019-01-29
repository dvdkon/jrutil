-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

CREATE OR REPLACE FUNCTION gtfs_interval(i INTERVAL) RETURNS TEXT
LANGUAGE SQL AS $$
    SELECT (date_part('epoch', i) * INTERVAL '1 second')::TEXT;
$$;

COPY $schema.agencies
TO PROGRAM 'cat >> $outpath/agency.txt' WITH (FORMAT csv);

COPY $schema.stops
TO PROGRAM 'cat >> $outpath/stops.txt' WITH (FORMAT csv);

COPY $schema.routes
TO PROGRAM 'cat >> $outpath/routes.txt' WITH (FORMAT csv);

COPY $schema.trips
TO PROGRAM 'cat >> $outpath/trips.txt' WITH (FORMAT csv);

COPY (
    SELECT tripid, gtfs_interval(arrivaltime), gtfs_interval(departuretime),
           stopid, stopsequence, headsign, pickuptype, dropofftype,
           shapedisttraveled, timepoint
    FROM $schema.stoptimes
) TO PROGRAM 'cat >> $outpath/stop_times.txt' WITH (FORMAT csv);

COPY (
    SELECT id,
           CASE WHEN weekdayservice[0] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[1] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[2] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[3] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[4] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[5] THEN 1 ELSE 0 END,
           CASE WHEN weekdayservice[6] THEN 1 ELSE 0 END,
           startdate, enddate
    FROM $schema.calendar
) TO PROGRAM 'cat >> $outpath/calendar.txt' WITH (FORMAT csv);

COPY $schema.calendarexceptions
TO PROGRAM 'cat >> $outpath/calendar_dates.txt' WITH (FORMAT csv);
