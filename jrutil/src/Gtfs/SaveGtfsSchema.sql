-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

-- Template arguments:
--     "schema" - SQL schema containing the GTFS tables
--     "outpath" - Output path for the CSV files
--     "platform" - Either "unix" or "windows"

{{ func copy_action
   filename = $0
}}

{{- if platform == "unix" -}} PROGRAM 'cat >> #outpath/#filename'
{{- else if platform == "windows" -}} '#outpath\#filename'
{{- end -}}

{{ end }}

CREATE OR REPLACE FUNCTION gtfs_interval(i INTERVAL) RETURNS TEXT
LANGUAGE SQL AS $$
    SELECT (date_part('epoch', i) * INTERVAL '1 second')::TEXT;
$$;

COPY #schema.agencies
TO {{ copy_action "agency.txt" }} WITH (FORMAT csv);

COPY #schema.stops
TO {{ copy_action "stops.txt" }} WITH (FORMAT csv);

COPY #schema.routes
TO {{ copy_action "routes.txt" }} WITH (FORMAT csv);

COPY #schema.trips
TO {{ copy_action "trips.txt" }} WITH (FORMAT csv);

COPY (
    SELECT tripid, gtfs_interval(arrivaltime), gtfs_interval(departuretime),
           stopid, stopsequence, headsign, pickuptype, dropofftype,
           shapedisttraveled, timepoint, stopzoneids
    FROM #schema.stoptimes
) TO {{ copy_action "stop_times.txt" }} WITH (FORMAT csv);

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
    FROM #schema.calendar
) TO {{ copy_action "calendar.txt" }} WITH (FORMAT csv);

COPY #schema.calendarexceptions
TO {{ copy_action "calendar_dates.txt" }} WITH (FORMAT csv);
