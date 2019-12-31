-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

SET pg_trgm.similarity_threshold = 0.8;

{{ func matches_with
   source = $0
   condition = $1
}}
    SELECT stops.id, gd.lat, gd.lon
    FROM #gtfs.stops
    INNER JOIN czptt_stops_geodata AS gd
        {{ if condition == "name" }}
            ON stops.name % gd.name
        {{ else if condition == "sr70" }}
            ON stops.id = '-CZPTTST-CZ-' || gd.sr70
            -- Stop parts (individual platforms) get the same coordinates for
            -- now
            OR stops.id LIKE '-CZPTTS-CZ-' || gd.sr70 || '%'
        {{ end }}
    WHERE gd.source = '#source'
{{ end }}

WITH
    matches AS (
        SELECT *, 1 AS priority
        FROM ({{ matches_with "osm" "sr70" }}) AS m
        UNION
        SELECT *, 2 AS priority
        FROM ({{ matches_with "osm" "name" }}) AS m
               UNION
        SELECT *, 3 AS priority
        FROM ({{ matches_with "external" "sr70" }}) AS m
        UNION
        SELECT *, 4 AS priority
        FROM ({{ matches_with "external" "name" }}) AS m
    ),
    matches_uniq AS (
        SELECT DISTINCT ON (id) *
        FROM matches
        ORDER BY id, priority
    )
UPDATE #gtfs.stops
SET lat = m.lat,
    lon = m.lon
FROM matches_uniq AS m
WHERE stops.id = m.id;
