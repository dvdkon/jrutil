// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

namespace RtView

open System.Threading
open WebSharper
open Npgsql

open JrUtil.SqlRecordStore

module ServerGlobals =
    let mutable dbConnStr = Unchecked.defaultof<string>
    let getDbConn () = getPostgresqlConnection dbConnStr

    let mutable routeSummariesRefreshTimer = Unchecked.defaultof<Timer>

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
                        routeId,
                        -- Get the most current trip
                        -- Might become a problem when looking at really old data
                        (array_agg(tripId ORDER BY tripStartDate DESC))[1] AS tripId,
                        MIN(tripStartDate) AS firstDate,
                        MAX(tripStartDate) AS lastDate,
                        COALESCE(
                            (array_agg(routeShortName ORDER BY tripStartDate DESC))[1],
                            (array_agg(routeId ORDER BY tripStartDate DESC))[1]
                        ) AS name
                    FROM tripDetails AS td
                    GROUP BY routeId)
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
                FROM routes AS r
            """ []

        routeSummariesRefreshTimer <- new Timer((fun _ ->
            let conn = getDbConn ()
            executeSql conn "REFRESH MATERIALIZED VIEW routeSummaries" []
        // Fire first after 10 minutes, then every 10 minutes
        ), null, 10*60*1000, 10*60*1000)

    let init dbConnStr_ =
        dbConnStr <- dbConnStr_
        use c = getDbConn ()
        createSqlViews c

[<JavaScript>]
module ClientGlobals =
    open WebSharper.UI

    type Locations =
        | [<EndPoint "/">] Homepage
        | [<Query("fromDate", "toDate", "search")>]
          Routes of fromDate: string option
                  * toDate: string option
                  * search: string option
        | [<Query("fromDate", "toDate")>]
          Trips of tripId: string
                 * fromDate: string option
                 * toDate: string option
        | Trip of tripId: string * date: string

    let mutable router = Unchecked.defaultof<Sitelets.Router<Locations>>
    let mutable location = Unchecked.defaultof<Var<Locations>>
    let setLocation newLoc =
        if location.Value <> newLoc then location.Value <- newLoc
