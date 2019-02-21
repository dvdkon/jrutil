-- This file is part of JrUtil and is licenced under the GNU GPLv3 or later
-- (c) 2019 David Koňařík

-- The DPMLJ GTFS feed has duplicate routes. I suspect it's to differentiate
-- different timetables for different days. it's however mostly just confusing.

UPDATE trips
SET routeid = (
    SELECT id FROM routes
    WHERE shortname = (SELECT shortname FROM routes
                        WHERE id = routeid)
    ORDER BY id -- Just some random ordering
    LIMIT 1);

-- Clean up useless routes created by the above UPDATE
DELETE FROM routes
WHERE NOT EXISTS (SELECT FROM trips WHERE routeid = routes.id);
