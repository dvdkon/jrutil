// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
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
            tripId, tripStartDate, stopId, tripStopIndex);
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

let insertTripPosition =
    let stopHistoryInserter =
        getSqlInserterTemplated
            typeof<DbStopHistoryItem>
            (sprintf """INSERT INTO "%s" (%s) VALUES %s
                        ON CONFLICT (tripId,
                                     tripStartDate,
                                     stopId,
                                     tripStopIndex) DO UPDATE
                            SET timezone = excluded.timezone,
                                arrivedAt = excluded.arrivedAt,
                                shouldArriveAt = excluded.shouldArriveAt,
                                departedAt = excluded.departedAt,
                                shouldDepartAt = excluded.shouldDepartAt
                                """)
            "stopHistory"
    let tripDetailsInserter =
        getSqlInserterTemplated
            typeof<DbTripDetail>
            (sprintf """INSERT INTO "%s" (%s) VALUES %s
                        ON CONFLICT (tripId, tripStartDate) DO UPDATE
                        SET routeId = excluded.routeId,
                            shortName = excluded.shortName""")
            "tripDetails"
    fun (conn: NpgsqlConnection) (tripPos: TripPosition) ->
        tripPos.coords |> Option.iter (fun coords ->
            sqlInsert conn "coordHistory" [{
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
                |> Array.map (fun item -> box {
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
                stopHistoryInserter conn rows
            with
            | e -> Log.Error(e, "Failed inserting {StopHistoryRows}", (sprintf "%A" rows))
        )
        tripDetailsInserter conn [{
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
            newRange daterange;
            existingDateRange daterange;
        BEGIN
            newRange := daterange(validDate, validDate, '[]');
            SELECT validDateRange
            INTO existingDateRange
            FROM stops
            WHERE (validDateRange -|- newRange OR validDateRange @> validDate)
              AND id = id_ AND name = name_ AND lat = lat_ AND lon = lon_;
            IF existingDateRange IS NOT NULL THEN
                UPDATE stops
                SET validDateRange = range_merge(validDateRange, newRange)
                WHERE id = id_
                  AND validDateRange = existingDateRange;
            ELSE
                INSERT INTO stops (id, validDateRange, name, lat, lon)
                VALUES (id_, newRange, name_, lat_, lon_);
            END IF;
        END;
        $$""" []
    stops |> Seq.iter (fun s ->
        executeSql conn """
            CALL upsert_stop(@id, @date, @name, @lat, @lon);
        """ ["id", box s.id; "date", box s.date; "name", box s.name;
             "lat", box s.lat; "lon", box s.lon]
    )
