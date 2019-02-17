-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2018 David Koňařík

-- This script is supposed to be templated by JrUtil.Gtfs.GtfsMerge.templateSql
-- It takes three arguments:
--     Two schemas with GTFS tables already present:
--         "merged" - The resultant GTFS feed, the result of merging
--         "in" - The individual GTFS feed to be merged
--     "feednum" - A unique identifier for the current input feed
-- Its searchpath should consist of a "throwaway" schema which will only be
-- used for temporary tables (ID maps).

-- JrUtil's merge functionality started with a goal of reasonably merging any
-- number of arbitrary ("correct") GTFS feeds. As it was changed and rewritten,
-- it became clear that this goal is unrealistic, so the only functioning
-- versions were only actually able to merge feeds which were very similar in
-- structure (basically everything names the same). I have now officially given
-- up. This version of the merge function now mostly relies on inputs having
-- "stable IDs" -- whether two rows should be merged is determined by the
-- equality of their IDs.

CREATE OR REPLACE FUNCTION create_idmap_table(name TEXT) RETURNS void
LANGUAGE plpgsql AS $func$
BEGIN
    EXECUTE format($$
        DROP TABLE IF EXISTS %I;
        CREATE TEMPORARY TABLE %I (
            orig_id TEXT PRIMARY KEY,
            new_id TEXT NOT NULL,
            merged boolean NOT NULL
        );
    $$, name, name);
END;
$func$;

CREATE OR REPLACE FUNCTION
id_is_stable(id text) RETURNS boolean
IMMUTABLE LANGUAGE SQL AS $$
        SELECT id LIKE '-%';
$$;

CREATE OR REPLACE FUNCTION
get_new_id(old_id text, new_id text) RETURNS text
IMMUTABLE LANGUAGE SQL AS $$
    SELECT CASE WHEN id_is_stable(old_id) THEN old_id
                ELSE COALESCE(new_id, '$feednum/' || old_id)
           END;
$$;

CREATE OR REPLACE FUNCTION
populate_idmap(idmap TEXT, name TEXT, merge_func TEXT) RETURNS void
LANGUAGE plpgsql AS $func$
BEGIN
    EXECUTE format($$
        INSERT INTO %I
            SELECT old.id,
                   get_new_id(old.id, new.id),
                   new.id IS NOT NULL
            FROM $in.%I AS old
            LEFT JOIN $merged.%I AS new ON
                CASE WHEN id_is_stable(old.id) THEN old.id = new.id
                     ELSE %I(old, new)
                END;
    $$, idmap, name, name, merge_func);
END;
$func$;

-- If there are multiple mergable items in the same GTFS feed it will cause
-- problems down the line (manifested as uniqueness constraint problems)

--DROP FUNCTION IF EXISTS agency_should_merge;
CREATE OR REPLACE FUNCTION
agency_should_merge(a1 $in.agencies, a2 $merged.agencies) RETURNS boolean
IMMUTABLE LANGUAGE SQL AS $$
    SELECT a1.name = a2.name;
$$;

--DROP FUNCTION IF EXISTS stop_should_merge;
CREATE OR REPLACE FUNCTION
stop_should_merge(s1 $in.stops, s2 $merged.stops) RETURNS boolean
LANGUAGE SQL AS $$
    SELECT
        s1.name = s2.name
            -- Note to self: Comparing anything to null results in null,
            -- making everything null and therefore "falsy". Reminds me of
            -- PHP, but worse somehow...
            AND COALESCE(s1.locationtype, '0') = COALESCE(s2.locationtype, '0')
            AND (COALESCE(s1.locationtype, '0') NOT IN ('', '0')
                OR s1.platformcode IS NOT DISTINCT FROM s2.platformcode);
$$;

--DROP FUNCTION IF EXISTS route_should_merge;
CREATE OR REPLACE FUNCTION
route_should_merge(r1 $in.routes, r2 $merged.routes) RETURNS boolean
LANGUAGE SQL AS $$
    SELECT r1.shortname = r2.shortname;
$$;

CREATE OR REPLACE FUNCTION
trip_should_merge(t1 $in.trips, t2 $merged.trips) RETURNS boolean
LANGUAGE SQL AS $$
    SELECT false;
$$;

-- Agencies:

SELECT create_idmap_table('agency_idmap');

-- Some JDF batches have agencies that only differ in address and not in other
-- attributes. Since this script merges agencies by name, only use the first
-- one
INSERT INTO agency_idmap
    SELECT old.id, get_new_id(uniq.id, new.id),
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
           locationtype, idmap_ps.new_id, timezone, wheelchairboarding,
           platformcode
    FROM $in.stops
    LEFT JOIN stop_idmap AS idmap ON idmap.orig_id = id
    LEFT JOIN stop_idmap AS idmap_ps ON idmap_ps.orig_id = parentstation
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

SELECT populate_idmap('trip_idmap', 'trips', 'trip_should_merge');

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
