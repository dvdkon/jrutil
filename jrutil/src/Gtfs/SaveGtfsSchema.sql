-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2018 David Koňařík

COPY $schema.agencies
TO PROGRAM 'cat >> $outpath/agency.txt' WITH (FORMAT csv);

COPY $schema.stops
TO PROGRAM 'cat >> $outpath/stop.txt' WITH (FORMAT csv);

COPY $schema.routes
TO PROGRAM 'cat >> $outpath/route.txt' WITH (FORMAT csv);

COPY $schema.trips
TO PROGRAM 'cat >> $outpath/trip.txt' WITH (FORMAT csv);

COPY $schema.stoptimes
TO PROGRAM 'cat >> $outpath/stop_times.txt' WITH (FORMAT csv);

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
