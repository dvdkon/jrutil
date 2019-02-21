-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

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
                ELSE COALESCE(new_id, '#feednum/' || old_id)
           END;
$$;

CREATE OR REPLACE FUNCTION
populate_idmap(idmap TEXT, name TEXT, merge_func TEXT) RETURNS void
LANGUAGE plpgsql AS $func$
BEGIN
    EXECUTE format($$
        INSERT INTO %1$I
            SELECT DISTINCT ON (old_id) old_id, new_id, merged
            FROM (SELECT old.id AS old_id,
                         old.id AS new_id,
                         new.id IS NOT NULL AS merged,
                         8192 AS score
                  FROM #in.%2$I AS old
                  LEFT JOIN #merged.%2$I AS new ON
                      old.id = new.id
                  WHERE id_is_stable(old.id)
                  UNION
                  SELECT old.id,
                         COALESCE(new.id, '#feednum/' || old.id),
                         new.id IS NOT NULL,
                         %3$I(old, new)
                  FROM #in.%2$I AS old
                  LEFT JOIN #merged.%2$I AS new ON
                       %3$I(old, new) > 0) AS candidates
            ORDER BY old_id, score DESC;
    $$, idmap, name, merge_func);
END;
$func$;

-- If there are multiple mergable items in the same GTFS feed it will cause
-- problems down the line (manifested as uniqueness constraint problems)

CREATE OR REPLACE FUNCTION
agency_should_merge(a1 #in.agencies, a2 #merged.agencies) RETURNS integer
IMMUTABLE LANGUAGE SQL AS $$
    SELECT CASE WHEN a1.name = a2.name THEN 4096 ELSE 0 END;
$$;

CREATE OR REPLACE FUNCTION
stop_should_merge(s1 #in.stops, s2 #merged.stops) RETURNS integer
IMMUTABLE LANGUAGE SQL AS $$
    SELECT CASE
        WHEN (s1.name = s2.name
              -- Note to self: Comparing anything to null results in null,
              -- making everything null and therefore "falsy". Reminds me of
              -- PHP, but worse somehow...
              AND COALESCE(s1.locationtype, '0')
                  = COALESCE(s2.locationtype, '0')
              AND (COALESCE(s1.locationtype, '0') NOT IN ('', '0')
                  OR s1.platformcode IS NOT DISTINCT FROM s2.platformcode)
            AND ((s1.lat IS NULL AND s1.lon IS NULL)
                  OR (s2.lat IS NULL AND s2.lon IS NULL))) THEN 2048
        -- TODO: We'll probably have to both have a treshold and mutliple
        -- sorted candidates
        WHEN (s1.lat = s2.lat AND s1.lon = s2.lon) THEN 4096
        ELSE 0
    END;
$$;

CREATE OR REPLACE FUNCTION
route_should_merge(r1 #in.routes, r2 #merged.routes) RETURNS integer
LANGUAGE SQL IMMUTABLE AS $$
    SELECT CASE WHEN r1.longname = r2.longname THEN 4096 ELSE 0 END;
$$;

-- Agencies:

SELECT create_idmap_table('agency_idmap');

-- Some JDF batches have agencies that only differ in address and not in other
-- attributes. Since this script merges agencies by name, only use the first
-- one
INSERT INTO agency_idmap
    SELECT old.id, get_new_id(uniq.id, new.id),
           new.id IS NOT NULL OR old.id <> uniq.id
    FROM #in.agencies AS old
    LEFT JOIN (SELECT DISTINCT ON (name) * FROM #in.agencies) AS uniq
        ON old.name = uniq.name
    LEFT JOIN #merged.agencies AS new ON agency_should_merge(uniq, new) > 0.1;

-- SELECT populate_idmap('agency_idmap', 'agencies', 'agency_should_merge');

INSERT INTO #merged.agencies
    SELECT idmap.new_id, name, url, timezone, lang, phone, fareUrl, email
    FROM #in.agencies
    LEFT JOIN agency_idmap AS idmap ON id = idmap.orig_id
    WHERE NOT idmap.merged;

-- Stops:

SELECT create_idmap_table('stop_idmap');

SELECT populate_idmap('stop_idmap', 'stops', 'stop_should_merge');

-- TODO: Merge zoneid
INSERT INTO #merged.stops
    SELECT idmap.new_id, code, name, description, lat, lon, zoneid, url,
           locationtype, idmap_ps.new_id, timezone, wheelchairboarding,
           platformcode
    FROM #in.stops
    LEFT JOIN stop_idmap AS idmap ON idmap.orig_id = id
    LEFT JOIN stop_idmap AS idmap_ps ON idmap_ps.orig_id = parentstation
    WHERE NOT idmap.merged;

-- Routes:

SELECT create_idmap_table('route_idmap');

SELECT populate_idmap('route_idmap', 'routes', 'route_should_merge');

INSERT INTO #merged.routes
    SELECT route_idmap.new_id, agency_idmap.new_id, shortname, longname,
           description, routetype, url, color, textcolor, sortorder
    FROM #in.routes
    LEFT JOIN route_idmap ON id = route_idmap.orig_id
    LEFT JOIN agency_idmap ON agencyid = agency_idmap.orig_id
    WHERE NOT route_idmap.merged;

-- Calendar:

SELECT create_idmap_table('calendar_idmap');

-- TODO: Don't insert calendar entries used only for non-inserted trips
INSERT INTO calendar_idmap
    SELECT DISTINCT ON (old_id) old_id, new_id, merged FROM (
        SELECT id AS old_id, '#feednum/' || id AS new_id, false AS merged
        FROM #in.calendar
        UNION
        SELECT id AS old_id, '#feednum/' || id AS new_id, false AS merged
        FROM #in.calendarexceptions
    ) AS ids;

INSERT INTO #merged.calendar
    SELECT new_id, weekdayservice, startdate, enddate
    FROM #in.calendar
    LEFT JOIN calendar_idmap ON id = orig_id;

INSERT INTO #merged.calendarexceptions
    SELECT new_id, date, exceptiontype
    FROM #in.calendarexceptions
    LEFT JOIN calendar_idmap ON id = orig_id;

-- Trips:

{{ func trip_time_boundary # Args: schema, which end, trip id
}}
    SELECT COALESCE(arrivaltime, departuretime)
    FROM {{$0}}.stoptimes
    WHERE tripid = {{$2}}
    ORDER BY arrivaltime, departuretime
        {{ if $1 == "start" }} ASC
        {{ else }} DESC {{ end }}
    LIMIT 1
{{ end }}

{{ func common_cal_bitmap # Args: schema, trip id, schema2, trip id2
}}
    SELECT array_agg(
        (SELECT weekdayservice FROM {{$0}}.calendar
            WHERE id = {{$1}})[EXTRACT(DOW FROM d) + 1]
        OR EXISTS (SELECT FROM {{$0}}.calendarexceptions
                   WHERE id = {{$1}} AND exceptiontype = '1' AND date = d)
        AND NOT EXISTS (SELECT FROM {{$0}}.calendarexceptions
                        WHERE id = {{$1}}
                            AND exceptiontype = '2' AND date = d))
    FROM generate_series(
        greatest(
            (SELECT startdate FROM {{$0}}.calendar WHERE id = {{$1}}),
            (SELECT startdate FROM {{$2}}.calendar WHERE id = {{$3}})),
        least(
            (SELECT enddate FROM {{$0}}.calendar WHERE id = {{$1}}),
            (SELECT enddate FROM {{$2}}.calendar WHERE id = {{$3}})),
        '1 day'::interval) AS d
{{ end }}

CREATE OR REPLACE FUNCTION
trip_should_merge(t1 #in.trips, t2 #merged.trips) RETURNS integer
IMMUTABLE LANGUAGE SQL AS $$
    SELECT CASE
        WHEN (SELECT new_id FROM route_idmap
              WHERE orig_id = t1.routeid) = t2.routeid
             AND ({{ trip_time_boundary in "start" "t1.id" }})
               = ({{ trip_time_boundary merged "start" "t2.id" }})
             AND ({{ trip_time_boundary in "end" "t1.id" }})
               = ({{ trip_time_boundary merged "end" "t2.id" }})
             AND ({{ common_cal_bitmap in "t1.id" merged "t2.id" }})
               = ({{ common_cal_bitmap merged "t2.id" in "t1.id" }})
            THEN 4096
        ELSE 0
    END
$$;

SELECT create_idmap_table('trip_idmap');

SELECT populate_idmap('trip_idmap', 'trips', 'trip_should_merge');

INSERT INTO #merged.trips
    SELECT route_idmap.new_id, calendar_idmap.new_id, trip_idmap.new_id,
           headsign, shortname, directionid, blockid, shapeid,
           wheelchairaccessible, bikesallowed
    FROM #in.trips
    LEFT JOIN route_idmap ON route_idmap.orig_id = routeid
    LEFT JOIN calendar_idmap ON calendar_idmap.orig_id = serviceid
    LEFT JOIN trip_idmap ON trip_idmap.orig_id = id;

-- StopTimes:

INSERT INTO #merged.stoptimes
    SELECT trip_idmap.new_id, arrivaltime, departuretime, stop_idmap.new_id,
           stopsequence, headsign, pickuptype, dropofftype, shapedisttraveled,
           timepoint, stopzoneids
    FROM #in.stoptimes
    LEFT JOIN trip_idmap ON trip_idmap.orig_id = tripid
    LEFT JOIN stop_idmap ON stop_idmap.orig_id = stopid
    WHERE NOT trip_idmap.merged;
