-- This script is supposed to be templated by JrUtil.Gtfs.GtfsMerge.templateSql
-- It takes three arguments:
--     Two schemas with GTFS tables already present:
--         "merged" - The resultant GTFS feed, the result of merging
--         "in" - The individual GTFS feed to be merged
--     "feednum" - A unique identifier for the current input feed
-- Its searchpath should consist of a "throwaway" schema which will only be
-- used for temporary tables (ID maps).

CREATE OR REPLACE FUNCTION create_idmap_table(name TEXT) RETURNS VOID
LANGUAGE plpgsql AS $func$
BEGIN
    EXECUTE format($$
        DROP TABLE IF EXISTS %I;
        CREATE TEMPORARY TABLE %I (
            orig_id TEXT PRIMARY KEY,
            new_id TEXT NOT NULL,
            merged BOOLEAN NOT NULL
        );
    $$, name, name);
END;
$func$;

CREATE OR REPLACE FUNCTION
populate_idmap(idmap TEXT, name TEXT, merge_func TEXT) RETURNS VOID
LANGUAGE plpgsql AS $func$
BEGIN
    EXECUTE format($$
        INSERT INTO %I
            SELECT old.id, COALESCE(new.id, '$feednum/' || old.id), new.id IS NOT NULL
            FROM $in.%I AS old
            LEFT JOIN $merged.%I AS new ON %I(old, new);
    $$, idmap, name, name, merge_func);
END;
$func$;

-- If there are multiple mergable items in the same GTFS feed it will cause
-- problems down the line (manifested as uniqueness constraint problems)

DROP FUNCTION IF EXISTS agency_should_merge;
CREATE FUNCTION
agency_should_merge(a1 $in.agencies, a2 $merged.agencies) RETURNS BOOLEAN
LANGUAGE SQL AS $$
    SELECT a1.name = a2.name;
$$;

DROP FUNCTION IF EXISTS stop_should_merge;
CREATE FUNCTION
stop_should_merge(s1 $in.stops, s2 $merged.stops) RETURNS BOOLEAN
LANGUAGE SQL AS $$
    {{ if check_stations }}
        SELECT s1.name = s2.name
            -- TODO: '' should be equal to '0'
            AND s1.locationtype = s2.locationtype
            AND (s1.locationtype NOT IN (NULL, '', '0')
                OR s1.platformcode = s2.platformcode);
    {{ else }}
        SELECT s1.name = s2.name;
    {{ end }}
$$;

DROP FUNCTION IF EXISTS route_should_merge;
CREATE FUNCTION
route_should_merge(r1 $in.routes, r2 $merged.routes) RETURNS BOOLEAN
LANGUAGE SQL AS $$
    SELECT r1.shortname = r2.shortname;
$$;

-- Agencies:

SELECT create_idmap_table('agency_idmap');

-- Some JDF batches have agencies that only differ in address and not in other
-- attributes. Since this script merges agencies by name, only use the first
-- one
INSERT INTO agency_idmap
    SELECT old.id, COALESCE(new.id, '$feednum/' || uniq.id),
           new.id IS NOT NULL OR old.id <> uniq.id
    FROM $in.agencies AS old
    LEFT JOIN (SELECT DISTINCT ON (name) * FROM $in.agencies) AS uniq
        ON old.name = uniq.name
    LEFT JOIN $merged.agencies AS new ON agency_should_merge(uniq, new);

-- SELECT populate_idmap('agency_idmap', 'agencies', 'agency_should_merge');

INSERT INTO $merged.agencies
    SELECT idmap.new_id, name, url, timezone, lang, phone, fareUrl, email
    FROM $in.agencies
    LEFT JOIN agency_idmap AS idmap ON id = idmap.orig_id
    WHERE NOT idmap.merged;

-- Stops:

SELECT create_idmap_table('stop_idmap');

SELECT populate_idmap('stop_idmap', 'stops', 'stop_should_merge');

INSERT INTO $merged.stops
    SELECT idmap.new_id, code, name, description, lat, lon, zoneid, url,
           locationtype, parentstation, timezone, wheelchairboarding,
           platformcode
    FROM $in.stops
    LEFT JOIN stop_idmap AS idmap ON id = idmap.orig_id
    WHERE NOT idmap.merged;

-- Routes:

SELECT create_idmap_table('route_idmap');

SELECT populate_idmap('route_idmap', 'routes', 'route_should_merge');

INSERT INTO $merged.routes
    SELECT route_idmap.new_id, agency_idmap.new_id, shortname, longname,
           description, routetype, url, color, textcolor, sortorder
    FROM $in.routes
    LEFT JOIN route_idmap ON id = route_idmap.orig_id
    LEFT JOIN agency_idmap ON agencyid = agency_idmap.orig_id
    WHERE NOT route_idmap.merged;

-- Calendar:
-- TODO: Merge calendar entries

SELECT create_idmap_table('calendar_idmap');

INSERT INTO calendar_idmap
    SELECT DISTINCT ON (old_id) old_id, new_id, merged FROM (
        SELECT id AS old_id, '$feednum/' || id AS new_id, false AS merged
        FROM $in.calendar
        UNION
        SELECT id AS old_id, '$feednum/' || id AS new_id, false AS merged
        FROM $in.calendarexceptions
    ) AS ids;

INSERT INTO $merged.calendar
    SELECT new_id, weekdayservice, startdate, enddate
    FROM $in.calendar
    LEFT JOIN calendar_idmap ON id = orig_id;

INSERT INTO $merged.calendarexceptions
    SELECT new_id, date, exceptiontype
    FROM $in.calendarexceptions
    LEFT JOIN calendar_idmap ON id = orig_id;

-- Trips:
-- TODO: Merge trips

SELECT create_idmap_table('trip_idmap');

INSERT INTO trip_idmap
    SELECT id, '$feednum/' || id, false
    FROM $in.trips;

INSERT INTO $merged.trips
    SELECT route_idmap.new_id, calendar_idmap.new_id, trip_idmap.new_id,
           headsign, shortname, directionid, blockid, shapeid,
           wheelchairaccessible, bikesallowed
    FROM $in.trips
    LEFT JOIN route_idmap ON route_idmap.orig_id = routeid
    LEFT JOIN calendar_idmap ON calendar_idmap.orig_id = serviceid
    LEFT JOIN trip_idmap ON trip_idmap.orig_id = id;

-- StopTimes:

INSERT INTO $merged.stoptimes
    SELECT trip_idmap.new_id, arrivaltime, departuretime, stop_idmap.new_id,
           stopsequence, headsign, pickuptype, dropofftype, shapedisttraveled,
           timepoint
    FROM $in.stoptimes
    LEFT JOIN trip_idmap ON trip_idmap.orig_id = tripid
    LEFT JOIN stop_idmap ON stop_idmap.orig_id = stopid;
