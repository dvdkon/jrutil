// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

namespace RtView

open System.Text.Json
open Microsoft.AspNetCore.Mvc
open Giraffe.ViewEngine
open NodaTime

open JrUtil.SqlRecordStore
open JrUtil.Utils
open RtView.Utils
open RtView.ServerGlobals

type WebTripStop = {
    stopId: string
    stopName: string
    arrivedAt: string option
    shouldArriveAt: string option
    arrivalDelay: decimal option
    departedAt: string option
    shouldDepartAt: string option
    departureDelay: decimal option
}

type DelayChartItem = {
    x: float
    y: float
    stopName: string
}

type DelayChartLabel = {
    x: float
    label: string
}

type DelayChartData = {
    data: DelayChartItem array
    labels: DelayChartLabel array
}

[<Route("Trip/{tripId}/{tripStartDate}")>]
type TripController() =
    inherit ControllerBase()

    let getTripStops (tripId: string) (startDate: LocalDate) () =
        use c = getDbConn ()
        sqlQueryRec<WebTripStop> c """
            SELECT stopId, stopName,
                   to_char(arrivedAt, 'HH24:MI') AS arrivedAt,
                   to_char(shouldArriveAt, 'HH24:MI') AS shouldArriveAt,
                   EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt)/60 AS arrivalDelay,
                   to_char(departedAt, 'HH24:MI') AS departedAt,
                   to_char(shouldDepartAt, 'HH24:MI') as shouldDepartAt,
                   EXTRACT(EPOCH FROM departedAt - shouldDepartAt)/60 AS departureDelay
            FROM stopHistoryWithNames
            WHERE tripId = @tripId
              AND tripStartDate = @startDate::date
            ORDER BY tripStopIndex
            """ ["tripId", box tripId
                 "startDate", box startDate]
        |> Seq.toArray

    let getTripDelayChart (tripId: string) (startDate: LocalDate) () =
        use c = getDbConn ()
        let out =
            sqlQuery c """
WITH stopHist AS (
    SELECT *
    FROM stopHistoryWithNames
    WHERE tripId = @tripId
      AND tripStartDate = @startDate::date
), data AS (
    SELECT * FROM (
        -- Arrival delays
        SELECT
            EXTRACT(EPOCH FROM shouldArriveAt
                    - first_value(shouldDepartAt)
                        OVER (ORDER BY tripStopIndex))
                -- Guard against two+ stops in a row having the same x
                + (SELECT COUNT(*) * 10 FROM stopHist AS sh2
                   WHERE sh2.tripStopIndex < sh.tripStopIndex
                     AND sh2.shouldArriveAt = sh.shouldArriveAt) AS x,
               EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt)/60 AS delay,
               stopName || ' (arr.)' AS stopName
        FROM stopHist AS sh
        UNION
        -- Departure delays
        SELECT
            -- If scheduled arrival and departure are the same, shift departure by
            -- not to get conflicting xs
            EXTRACT(EPOCH FROM CASE
                WHEN shouldDepartAt = shouldArriveAt THEN
                    shouldDepartAt - first_value(shouldDepartAt)
                        OVER (ORDER BY tripStopIndex) + '1sec'::INTERVAL
                ELSE shouldDepartAt - first_value(shouldDepartAt)
                    OVER (ORDER BY tripStopIndex)
            END) + (SELECT COUNT(*) * 10 FROM stopHist AS sh2
                    WHERE sh2.tripStopIndex < sh.tripStopIndex
                      AND sh2.shouldDepartAt = sh.shouldDepartAt) AS x,
            EXTRACT(EPOCH FROM departedAt - shouldDepartAt)/60 AS delay,
            stopName || ' (dep.)' as stopName
        FROM stopHist AS sh
    ) AS i
    WHERE x IS NOT NULL AND delay IS NOT NULL
    ORDER BY x
), labels AS (
    (SELECT
        0 AS x,
        stopName  || ' (' || to_char(shouldDepartAt, 'HH24:MI')
            || ')' AS label
    FROM stopHist
    ORDER BY tripStopIndex
    LIMIT 1)
    UNION
    (SELECT
        EXTRACT(EPOCH FROM shouldArriveAt - first_value(shouldDepartAt)
            OVER (ORDER BY tripStopIndex)) AS x,
        stopName  || ' (' || to_char(shouldArriveAt, 'HH24:MI') || ')' AS label
    FROM stopHist
    ORDER BY tripStopIndex DESC
    LIMIT 1)
    UNION
    (SELECT
        COALESCE(
            EXTRACT(EPOCH FROM shouldArriveAt -
                (SELECT min(shouldDepartAt) FROM stopHist)),
            0) AS x,
        stopName || ' (' || to_char(shouldArriveAt, 'HH24:MI') || ')' AS label
    FROM stopHist
    WHERE shouldDepartAt <> shouldArriveAt
    ORDER BY shouldDepartAt - shouldArriveAt DESC
    LIMIT 10)
    ORDER BY x
)
SELECT
    (SELECT json_agg(json_build_object(
        'x', x, 'y', delay, 'stopName', stopName))
     FROM data) AS data,
    (SELECT json_agg(json_build_object(
        'x', x, 'label', label))
     FROM labels) AS labels;
                """ ["tripId", box tripId
                     "startDate", box startDate]
            |> Seq.exactlyOne
        $"""{{"data":{out.["data"]},"labels":{out.["labels"]}}}"""

    let stopRow (s: WebTripStop) =
        tr [] [
            td [] [str s.stopName]
            td [] [str (s.shouldArriveAt |> Option.defaultValue "")]
            td [] [str (s.arrivedAt |> Option.defaultValue "")]
            td [] [str (s.arrivalDelay
                         |> Option.map (sprintf "%.1f")
                         |> Option.defaultValue "")]
            td [] [str (s.shouldDepartAt |> Option.defaultValue "")]
            td [] [str (s.departedAt |> Option.defaultValue "")]
            td [] [str (s.departureDelay
                         |> Option.map (sprintf "%.1f")
                         |> Option.defaultValue "")]
        ]

    [<HttpGet>]
    member this.get(
            tripId: string,
            tripStartDate: LocalDate) =
        let tripName = getTripName tripId tripStartDate ()
        let stops = getTripStops tripId tripStartDate ()

        div [_class "trip-page"] [
            h1 [] [str $"Trip {tripName} on {dateToIso tripStartDate}"]
            table [] [
                thead [] [
                    th [] [str "Stop"]
                    th [] [str "Exp. arr."]
                    th [] [str "Arr."]
                    th [] [str "Arr. delay"]
                    th [] [str "Exp. dep."]
                    th [] [str "Dep."]
                    th [] [str "Dep. delay"]
                ]
                tbody [] [
                    for stop in stops -> stopRow stop
                ]
            ]
            div [_class "delay-chart"] [
                canvas [] []
            ]
        ]
        |> htmlResult $"Trip {tripName} on {dateToIso tripStartDate}"

    [<HttpGet("chartData")>]
    member this.chartData(
            tripId: string,
            tripStartDate: LocalDate) =
        this.Content(
            getTripDelayChart tripId tripStartDate (),
            "application/json")
