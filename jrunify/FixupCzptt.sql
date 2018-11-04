-- A script to modify GTFS feeds generated from CZPTT data to better fit into
-- JrUtil's merging process.

-- There are some stations in CZPTT that have the same name but are actually
-- different. This is fine by itself, but confuses the merging process, as
-- later stages have no choice but to match stations by name, as there's no
-- cross-format ID for stations and we want CZPTT stations to be mergable with
-- stations from other sources. This hack just finds all stations representing
-- border crossings and makes their names unique by appending the station ID
-- to them.

UPDATE stops
SET name = format('%s (%s)', name, id)
WHERE name = 'státní hranice';
