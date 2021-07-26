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
        | Trips of tripId: string
        | Trip of tripId: string * date: string

    let mutable router = Unchecked.defaultof<Sitelets.Router<Locations>>
    let mutable location = Unchecked.defaultof<Var<Locations>>
    let setLocation newLoc =
        if location.Value <> newLoc then location.Value <- newLoc
