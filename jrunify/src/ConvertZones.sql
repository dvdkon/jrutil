-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

-- This script will convert zone information to use stop_zone_ids
-- It uses lat and lon to distinguish equal stops.

UPDATE stoptimes
SET stopzoneids = (
    SELECT zoneid
    FROM stops
    WHERE stops.id = stopid);

CREATE TEMPORARY TABLE stop_idmap AS
    SELECT stops.id AS orig_id,
           uniq.id AS new_id,
           stops.id = uniq.id AS keep
    FROM stops
    LEFT JOIN stops AS uniq ON stops.lat = uniq.lat AND stops.lon = uniq.lon;

UPDATE stops
SET zoneid = (
    SELECT string_agg(zoneid, ', ')
    FROM stops AS s WHERE s.id = stop_idmap.orig_id;
)
FROM stop_idmap
WHERE new_id = id;

UPDATE stoptimes
SET stopid = (SELECT new_id FROM stop_idmap WHERE orig_id = stopid);

DELETE FROM stops
WHERE NOT (SELECT keep FROM stop_idmap WHERE orig_id = id);
