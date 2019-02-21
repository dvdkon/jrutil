# GTFS merge strategy

The merge process is divided into steps, each one merges data from an input
feed into the common "merged"/output feed.

Feeds are merged table(/file) by table. First, an "idmap table" is created,
which holds a mapping of IDs in the input feed onto IDs in the merged feed and
whether the row with the given ID should be copied into the output feed or
"merged into" another equivalent row.

The table's rows are then either ignored or copied into the output feed
according to the idmap table.

## Stable IDs

Stable IDs are IDs that will point to the same object (note that equality is
often hard to define) across feeds and across (a reasonable amount of) time.
Merges by stable IDs are preferred to other possible 

## Agencies

Agencies are merged if either their names match or if their stable IDs match.
This will no doubt lead to duplicate agencies unless manual work is performed,
since most formats don't have the necessary information to create stable IDs
for agencies.

## Routes

Routes are only merged based on stable IDs. Thankfully, stable IDs are easy to
create for trains and all other Czech public transport (using a six-digit ID
taken care of by the ministry of transportation). Equality is well-defined,
because these IDs are intended for public usage.

## Stops


Railway stops are just fine, defined by a country-wide list (see a pattern
here?).

Other stops are complicated. The ideal solution would be merging by
coordinates, but those are often not available. Czech stops have a sort of
canonical list of names as an export from the CIS JŘ system. This would make
merging easy were it not for the fact that most logical stops are further
divided into two or more physical locations (here referred to as "stop posts"),
often on opposite sides of a street. I have so far been unable to find a source
of stable IDs for stop posts, so merging those (without coordinates) would
require looking at the direction of all trips through that stop in the input
feed and trying to match them to equivalent trips in the merged feed. It might
actually be less work to add coordinates to all stop posts in datasets that
lack them.

## Trips

Trips are best merged by stable IDs, where they can exist. Another
possibility is merging by first merging routes, then looking at the routes
trips belong to and also at their start/stop times (from `stop_times`). We have
to take care not to merge trips that run at different days, though. That makes
merging trips pretty hard, because different sources are likely to start and
end their validity periods at different dates.

Some kind of "feed priority" system may have to be developed so that diversion
trips can create "holes" in regular trips and not the other way around.

## Calendar, calendar exceptions and stoptimes

These are never merged and only inserted if the related trip is. In essence,
trips are treated as opaque objects and merged/copied as a whole.
