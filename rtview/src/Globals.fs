// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

namespace RtView

open System.Threading
open Npgsql

open JrUtil.SqlRecordStore

module ServerGlobals =
    let mutable dbConnStr = None
    let getDbConn () = getPostgresqlConnection (Option.get dbConnStr)

    let routeSummariesRefreshTimer = new Timer((fun _ ->
        let conn = getDbConn ()
        executeSql
            conn
            "REFRESH MATERIALIZED VIEW CONCURRENTLY routeSummaries"
            []
    ))

    let createSqlViews conn =
        executeSql conn """
            CREATE OR REPLACE VIEW stopHistoryWithNames AS
                SELECT DISTINCT ON (tripId, tripStartDate,
                                    stopId, tripStopIndex)
                    sh.*, COALESCE(s.name, sub.name, stopId) AS stopName
                FROM stopHistory AS sh
                LEFT JOIN stops AS s ON
                s.id = sh.stopId AND s.validDateRange @> COALESCE(
                    sh.arrivedAt, sh.shouldArriveAt,
                    sh.departedAt, sh.shouldDepartAt)::date
                -- Stops, unbounded by validity range
                LEFT JOIN stops AS sub ON
                    sub.id = sh.stopId;
            """ []
        executeSql conn """
            DROP MATERIALIZED VIEW IF EXISTS routeSummaries;
            CREATE MATERIALIZED VIEW routeSummaries AS
                WITH routes AS (
                    SELECT
                        i1.routeId, i1.firstDate, i1.lastDate,
                        i2.tripId, i2.name
                    FROM (
                        SELECT
                            td.routeId,
                            MIN(td.tripStartDate) AS firstDate,
                            MAX(td.tripStartDate) AS lastDate
                        FROM tripdetails AS td
                        GROUP BY routeid
                    ) AS i1
                    LEFT JOIN (
                        SELECT DISTINCT ON (td2.routeId)
                            td2.routeId,
                            td2.tripId,
                            COALESCE(td2.routeShortName, td2.routeId) AS name
                        FROM tripdetails AS td2
                        ORDER BY routeId, tripStartDate DESC
                    ) AS i2 ON i2.routeId = i1.routeId
                )
                SELECT
                    routeId,
                    name,
                    firstDate,
                    lastDate,
                    (SELECT name FROM stophistory AS sh
                     LEFT JOIN stops AS s
                         ON s.id = sh.stopId
                         AND s.validDateRange @> COALESCE(
                             sh.arrivedAt,
                             sh.shouldArriveAt,
                             sh.departedAt,
                             sh.shouldDepartAt)::date
                     WHERE sh.tripId = r.tripId
                       AND sh.tripStartDate = r.lastDate
                     ORDER BY tripStopIndex LIMIT 1) AS firstStop,
                    (SELECT name FROM stophistory AS sh
                     LEFT JOIN stops AS s
                         ON s.id = sh.stopId
                         AND s.validDateRange @> COALESCE(
                             sh.arrivedAt,
                             sh.shouldArriveAt,
                             sh.departedAt,
                             sh.shouldDepartAt)::date
                     WHERE sh.tripId = r.tripId
                       AND sh.tripStartDate = r.lastDate
                     ORDER BY tripStopIndex DESC LIMIT 1) AS lastStop
                FROM routes AS r;
            CREATE UNIQUE INDEX ON routeSummaries (routeId);
            """ []

        executeSql conn """
            CREATE OR REPLACE FUNCTION startDates(_tripid text, _fromDate date, _toDate date, _tripStartDate date)
            RETURNS SETOF date LANGUAGE SQL AS $$
                SELECT tripStartDate
                FROM stopHistory
                WHERE tripId = _tripId
                  AND tripStartDate >= _fromDate
                  AND tripStartDate <= _toDate
                GROUP BY tripStartDate
                HAVING array_agg((stopId, shouldArriveAt::time, shouldDepartAt::time) ORDER BY tripStopIndex) = (
                    SELECT array_agg((stopId, shouldArriveAt::time, shouldDepartAt::time) ORDER BY tripStopIndex)
                    FROM stopHistory
                    WHERE tripId = _tripId
                      AND tripStartDate = _tripStartDate)
            $$
        """ []

        // Fire first after 60 minutes, then every 60 minutes
        routeSummariesRefreshTimer.Change(60*60*1000, 60*60*1000) |> ignore

    let init dbConnStr_ =
        dbConnStr <- Some dbConnStr_
        Thread(ThreadStart(fun () ->
            use c = getDbConn ()
            createSqlViews c
        )).Start()
