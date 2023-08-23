// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2020 David Koňařík

module JrUtil.RealTimeSql

open NodaTime
open Npgsql
open NpgsqlTypes
open Serilog

open JrUtil.SqlRecordStore
open JrUtil.RealTimeModel

// Db* types are meant for long-term relational storage, while the above types
// are more tuned to represent the structure usually found in APIs and should be
// easier to work with from F#

type DbCoordHistoryItem = {
    tripId: string
    tripStartDate: LocalDate
    observationTime: Instant
    lat: float
    lon: float
}

type DbStopHistoryItem = {
    tripId: string
    tripStartDate: LocalDate
    stopId: string
    tripStopIndex: int
    timeZone: DateTimeZone
    arrivedAt: LocalDateTime option
    shouldArriveAt: LocalDateTime option
    departedAt: LocalDateTime option
    shouldDepartAt: LocalDateTime option
}

type DbTripDetail = {
    tripId: string
    tripStartDate: LocalDate
    routeId: string
    shortName: string option
    // Route data doesn't belong here from a GTFS perspective, but since all the
    // APIs give trip-based data, this mirrors the source data
    routeShortName: string option
}

type DbStop = {
    id: string
    validDateRange: NpgsqlRange<LocalDate>
    name: string
    lat: float option
    lon: float option
}

let sqlCreateRealTimeTables conn =
    // TODO: Maybe adopt `table` func from Gtfs.fs?
    createTableFor conn typeof<DbCoordHistoryItem> "coordHistory"
    createTableFor conn typeof<DbStopHistoryItem> "stopHistory"
    createTableFor conn typeof<DbTripDetail> "tripDetails"
    createTableFor conn typeof<DbStop> "stops"
    executeSql conn """
        ALTER TABLE stopHistory ADD PRIMARY KEY (
            tripId, tripStartDate, tripStopIndex);
        CREATE INDEX ON stopHistory (tripId);
        CREATE INDEX ON stopHistory (tripStartDate);
        CREATE INDEX ON stopHistory (stopId);
        CREATE INDEX ON stopHistory (tripStopIndex);

        ALTER TABLE coordHistory ADD PRIMARY KEY (
            tripId, tripStartDate, observationTime);
        CREATE INDEX ON coordHistory (tripId);
        CREATE INDEX ON coordHistory (tripStartDate);
        CREATE INDEX ON coordHistory (observationTime);

        ALTER TABLE tripDetails ADD PRIMARY KEY (
            tripId, tripStartDate);
        CREATE INDEX ON tripDetails (tripId);
        CREATE INDEX ON tripDetails (tripStartDate);
        CREATE INDEX ON tripDetails (routeId);

        ALTER TABLE stops ADD PRIMARY KEY (id, validDateRange);
        CREATE INDEX ON stops (id);
        CREATE INDEX ON stops (validDateRange);
        CREATE INDEX ON stops (name);
    """ []

let stopHistoryInserter =
    getSqlInserterTemplated
        typeof<DbStopHistoryItem>
        (sprintf """INSERT INTO "%s" (%s) VALUES %s
                    ON CONFLICT (tripId,
                                 tripStartDate,
                                 tripStopIndex) DO UPDATE
                        SET stopId = excluded.stopId,
                            timezone = excluded.timezone,
                            arrivedAt = excluded.arrivedAt,
                            shouldArriveAt = excluded.shouldArriveAt,
                            departedAt = excluded.departedAt,
                            shouldDepartAt = excluded.shouldDepartAt
                            """)
        "stopHistory"

let insertStopHistory conn tripId tripStartDate (rows: DbStopHistoryItem seq) =
    stopHistoryInserter conn (Seq.map box rows)
    // When the count of reported stops decreases, we need to delete
    // the remaining higher-numbered stops
    // But not for empty histories, those are probably a bug
    if not <| Seq.isEmpty rows then
        executeSql conn """
            DELETE FROM stopHistory
            WHERE tripId = @tripId
              AND tripStartDate = @tripStartDate
              AND tripStopIndex > @maxStopIndex
            """ ["tripId", box tripId
                 "tripStartDate", box tripStartDate
                 "maxStopIndex",
                     box (rows
                          |> Seq.map (fun i -> i.tripStopIndex)
                          |> Seq.max)]

let insertCoordHistory conn items =
    getSqlInserterTemplated
        typeof<DbCoordHistoryItem>
        (sprintf """INSERT INTO "%s" (%s) VALUES %s
                    ON CONFLICT (tripId, tripStartDate, observationTime) DO UPDATE
                    SET lat = excluded.lat, lon = excluded.lon""")
        "coordHistory"
        conn
        (Seq.map box items)

let insertTripDetails conn details =
    getSqlInserterTemplated
        typeof<DbTripDetail>
        (sprintf """INSERT INTO "%s" (%s) VALUES %s
                    ON CONFLICT (tripId, tripStartDate) DO UPDATE
                    SET routeId = excluded.routeId,
                        shortName = excluded.shortName""")
        "tripDetails"
        conn
        (Seq.map box details)

let insertTripPosition =
    fun (conn: NpgsqlConnection) (tripPos: TripPosition) ->
        tripPos.coords |> Option.iter (fun coords ->
            insertCoordHistory conn [{
                DbCoordHistoryItem.tripId = tripPos.tripId
                tripStartDate = tripPos.tripStartDate
                observationTime = tripPos.observationTime
                lat = coords.lat
                lon = coords.lon
            }]
        )
        tripPos.stopHistory |> Option.iter (fun stopHistory ->
            let rows =
                stopHistory
                |> Array.map (fun item -> {
                    tripId = tripPos.tripId
                    tripStartDate = tripPos.tripStartDate
                    stopId = item.stopId
                    tripStopIndex = item.tripStopIndex
                    timeZone = item.timeZone
                    arrivedAt = item.arrivedAt
                    shouldArriveAt = item.shouldArriveAt
                    departedAt = item.departedAt
                    shouldDepartAt = item.shouldDepartAt
                })
            try
                insertStopHistory conn tripPos.tripId tripPos.tripStartDate rows
            with
            | e -> Log.Error(e, "Failed inserting {StopHistoryRows}", (sprintf "%A" rows))
        )
        insertTripDetails conn [{
            tripId = tripPos.tripId
            tripStartDate = tripPos.tripStartDate
            routeId = tripPos.routeId
            shortName = tripPos.shortName
            routeShortName = tripPos.routeShortName
        }]

let insertStops conn (stops: Stop seq) =
    executeSql conn """
        CREATE OR REPLACE PROCEDURE upsert_stop(
            id_ text, validDate date, name_ text, lat_ float8, lon_ float8)
        LANGUAGE plpgsql AS $$
        DECLARE
            newRange_ daterange;
            existing_ stops;
        BEGIN
            newRange_ := daterange(validDate, validDate, '[]');
            SELECT stops.*
            INTO existing_
            FROM stops
            WHERE (validDateRange -|- newRange_ OR validDateRange @> validDate)
              AND id = id_ AND name = name_;
            IF existing_ IS NOT NULL THEN
                IF existing_.lat = lat_ AND existing_.lon = lon_ THEN
                    UPDATE stops
                    SET validDateRange = range_merge(validDateRange, newRange_)
                    WHERE id = id_ AND validDateRange = existing_.validDateRange;
                ELSE
                    -- XXX: Will break if validDate is not on either end of
                    -- existing_.validDateRange
                    UPDATE stops
                    SET validDateRange = validDateRange - newRange_
                    WHERE id = id_ AND validDateRange = existing_.validDateRange;

                    INSERT INTO stops (id, validDateRange, name, lat, lon)
                    VALUES (id_, newRange_, name_, lat_, lon_)
                    ON CONFLICT (id, validDateRange) DO UPDATE SET
                        name = name_,
                        lat = lat_,
                        lon = lon_;
                END IF;
            ELSE
                INSERT INTO stops (id, validDateRange, name, lat, lon)
                VALUES (id_, newRange_, name_, lat_, lon_)
                ON CONFLICT (id, validDateRange) DO UPDATE SET
                    name = name_,
                    lat = lat_,
                    lon = lon_;

            END IF;
        END;
        $$""" []
    stops |> Seq.iter (fun s ->
        executeSql conn """
            CALL upsert_stop(@id, @date, @name, @lat, @lon);
        """ ["id", box s.id; "date", box s.date; "name", box s.name;
             "lat", box s.lat; "lon", box s.lon]
    )
