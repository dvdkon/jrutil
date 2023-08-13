-- This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
-- (c) 2020 David Koňařík

SET pg_trgm.similarity_threshold = 0.8;

{{ stddev_threshold = 0.003 }}

{{ func matches_with
   source = $0
}}
    WITH
        all_matches AS (
            SELECT stops.id, gd.lat, gd.lon
            FROM #gtfs.stops
            INNER JOIN other_stops_geodata AS gd
                ON stops.name % gd.name
            {{ if source != "_all" }}
                WHERE gd.source = '#source'
            {{ end }}
        ),
        -- Uses the first trip containg this stop, for simplicity and
        -- performance. When applied on JDF there's probably going to be just
        -- one route anyway
        first_stoptime AS (
            SELECT DISTINCT ON (stopid) *
            FROM #gtfs.stoptimes
        ),
        prev_stops AS (
            SELECT fst.stopid AS origin, m.lat, m.lon
            FROM first_stoptime AS fst
            LEFT JOIN #gtfs.stoptimes AS st ON st.tripid = fst.tripid
            LEFT JOIN all_matches AS m ON m.id = st.stopid
            WHERE st.stopsequence < fst.stopsequence
            ORDER BY fst.stopsequence - st.stopsequence
        ),
        prev_stop AS (
            SELECT DISTINCT ON (origin) origin, lat, lon
            FROM prev_stops
        ),
        next_stops AS (
            SELECT fst.stopid AS origin, m.lat, m.lon
            FROM first_stoptime AS fst
            LEFT JOIN #gtfs.stoptimes AS st ON st.tripid = fst.tripid
            LEFT JOIN all_matches AS m ON m.id = st.stopid
            WHERE st.stopsequence > fst.stopsequence
            ORDER BY st.stopsequence - fst.stopsequence
        ),
        next_stop AS (
            SELECT DISTINCT ON (origin) origin, lat, lon
            FROM next_stops
        ),
        match_from_avg AS (
            SELECT
                ps.origin AS id,
                (COALESCE(ps.lat, ns.lat) + COALESCE(ns.lat, ps.lat))/2 AS lat,
                (COALESCE(ps.lon, ns.lon) + COALESCE(ns.lon, ps.lon))/2 AS lon
            FROM prev_stop AS ps
            LEFT JOIN next_stop AS ns ON ns.origin = ps.origin
        ),
        match_by_avg AS (
            SELECT DISTINCT ON (mfa.id) mfa.id, am.lat, am.lon
            FROM match_from_avg AS mfa
            LEFT JOIN all_matches AS am ON am.id = mfa.id
            ORDER BY mfa.id, sqrt((am.lat - mfa.lat)^2 + (am.lon - mfa.lon)^2)
        )
    SELECT
        am.id,
        {{ for coord in ["lat", "lon"] }}
            CASE
                WHEN COUNT(*) <= 1 THEN AVG(#coord)
                WHEN (stddev_samp(lat) + stddev_samp(lon))/2
                     <= #stddev_threshold
                THEN AVG(#coord)
                ELSE
                    (SELECT #coord FROM match_by_avg WHERE id = am.id)
            END AS #coord
            {{ if coord == "lat" }} , {{ end }}
        {{ end }}
    FROM all_matches AS am
    GROUP BY am.id
{{ end }}

WITH
    matches AS (
        SELECT *, 1 AS priority
        FROM ({{ matches_with "osm" }}) AS m
        UNION
        SELECT *, 2 AS priority
        FROM ({{ matches_with "_all" }}) AS m
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
