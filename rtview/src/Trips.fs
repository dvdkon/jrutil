// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

namespace RtView

open System
open System.IO
open System.Text.Json
open System.Collections.Generic
open Microsoft.AspNetCore.Mvc
open Giraffe.ViewEngine
open NodaTime
open BigGustave

open JrUtil.Utils
open JrUtil.SqlRecordStore
open RtView.Utils
open RtView.ServerGlobals

type StopSeqGroup = {
    date: LocalDate // Acts as an ID
    dates: LocalDate array
}

type WebTripStopAgg = {
    stopId: string
    stopName: string
    shouldArriveAt: string option
    medArrivalDelay: float option
    p15ArrivalDelay: float option
    p85ArrivalDelay: float option
    shouldDepartAt: string option
    medDepartureDelay: float option
    p15DepartureDelay: float option
    p85DepartureDelay: float option
}

type DelayChartAvgItem = {
    x: float
    p15Delay: float
    y: float
    p85Delay: float
    stopName: string
}

type AggDelayChartLabel = {
    x: float
    label: string
}

type AggDelayChartData = {
    data: DelayChartAvgItem array
    labels: AggDelayChartLabel array
}

type TripDelayStop = {
    x: float
    label: string option
    dwellTime: float option
    arrival: bool
    departure: bool
}

[<Route("Trips/{tripId}")>]
type TripsController() =
    inherit ControllerBase()

    // Trips are divided into groups by their stop sequence, signified by the
    // tripStartDate of any trip of the group
    let getStopSeqGroups (tripId: string)
                         (fromDate: LocalDate) (toDate: LocalDate) () =
        use c = getDbConn ()
        sqlQueryRec<StopSeqGroup> c """
-- TODO: Less nested query
SELECT date AS date, dates AS dates
FROM (
    SELECT MIN(tripStartDate) AS date,
           COUNT(tripStartDate) AS c,
           array_agg(tripStartDate ORDER BY tripStartDate) AS dates
    FROM (
        SELECT
            -- TODO: Don't just cast to time, rather subtract starting day
            array_agg((stopId, shouldArriveAt::time, shouldDepartAt::time)
                ORDER BY tripStopIndex) AS stopSeq,
            tripStartDate
        FROM stopHistory
        WHERE tripId = @tripId
          AND tripStartDate >= @fromDate::date
          AND tripStartDate <= @toDate::date
        GROUP BY tripId, tripstartdate
    ) AS i2
    GROUP BY stopSeq
) AS i
ORDER BY c DESC
        """ ["tripId", box tripId
             "fromDate", box fromDate
             "toDate", box toDate]
        |> Seq.toArray

    let getTripStops (tripId: string)
                     (tripStartDate: LocalDate)
                     (fromDate: LocalDate) (toDate: LocalDate)
                     () =
        use c = getDbConn ()
        sqlQueryRec<WebTripStopAgg> c """
SELECT
    stopId, stopName,
    -- MIN() is here just for SQL semantics, all trips in an SSG should have identical planned times
    to_char(MIN(shouldArriveAt), 'HH24:MI') AS shouldArriveAt,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS medArrivalDelay,
    percentile_cont(0.15) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS p15ArrivalDelay,
    percentile_cont(0.85) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS p85ArrivalDelay,
    to_char(MIN(shouldDepartAt), 'HH24:MI') AS shouldDepartAt,
    percentile_cont(0.5) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS medDepartureDelay,
    percentile_cont(0.15) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS p15DepartureDelay,
    percentile_cont(0.85) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS p85DepartureDelay
FROM stopHistoryWithNames AS sh
WHERE tripId = @tripId
  AND EXISTS (SELECT FROM startDates(@tripId, @fromDate::date, @toDate::date, @tripStartDate::date) AS sd
              WHERE sd = sh.tripStartDate)
-- I know that there is exactly one stopId/stopName for tripStopIndex, but PostgreSQL doesn't
GROUP BY tripStopIndex, stopId, stopName
        """ ["tripId", box tripId
             "tripStartDate", box tripStartDate
             "fromDate", box fromDate
             "toDate", box toDate]
        |> Seq.toArray

    let getTripDelayChart (tripId: string)
                          (ssgDate: LocalDate)
                          (fromDate: LocalDate) (toDate: LocalDate)
                          () =
        use c = getDbConn ()
        let out =
            sqlQuery c """
WITH stopHist AS (
    SELECT
        stopId, stopName, tripStopIndex,
        MIN(shouldArriveAt) AS shouldArriveAt,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS medArrivalDelay,
        percentile_cont(0.15) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS p15ArrivalDelay,
        percentile_cont(0.85) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt))/60 AS p85ArrivalDelay,
        MIN(shouldDepartAt) AS shouldDepartAt,
        percentile_cont(0.5) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS medDepartureDelay,
        percentile_cont(0.15) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS p15DepartureDelay,
        percentile_cont(0.85) WITHIN GROUP (ORDER BY EXTRACT(EPOCH FROM departedAt - shouldDepartAt))/60 AS p85DepartureDelay
    FROM stopHistoryWithNames AS sh
    WHERE tripId = @tripId
      AND EXISTS (SELECT FROM startDates(@tripId, @fromDate::date, @toDate::date, @tripStartDate::date) AS sd
                  WHERE sd = sh.tripStartDate)
    GROUP BY tripStopIndex, stopId, stopName
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
            p15ArrivalDelay AS p15Delay,
            medArrivalDelay AS delay,
            p85ArrivalDelay AS p85Delay,
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
            p15DepartureDelay AS p15Delay,
            medDepartureDelay AS delay,
            p85DepartureDelay AS p85Delay,
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
        'x', x,
        'p15Delay', p15Delay,
        'y', delay,
        'p85Delay', p85Delay,
        'stopName', stopName))
     FROM data) AS data,
    (SELECT json_agg(json_build_object(
        'x', x, 'label', label))
     FROM labels) AS labels;
            """ ["tripId", box tripId
                 "tripStartDate", box ssgDate
                 "fromDate", box fromDate
                 "toDate", box toDate]
            |> Seq.exactlyOne
        $"""{{"data":{out.["data"]},"labels":{out.["labels"]}}}"""

    let getTripDelays (tripId: string)
                      (ssgDate: LocalDate)
                      (fromDate: LocalDate) (toDate: LocalDate)
                      () =
        use c = getDbConn ()
        let out =
            sqlQuery c """
WITH stopHist AS (
    SELECT
        tripStartDate, tripStopIndex, stopId, shouldArriveAt, shouldDepartAt,
        EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt) AS arrivalDelay,
        EXTRACT(EPOCH FROM departedAt - shouldDepartAt) AS departureDelay
    FROM stopHistory AS sh
    RIGHT JOIN startDates(@tripId, @fromDate::date, @toDate::date, @tripStartDate::date) AS sd
        ON sd = sh.tripStartDate
    WHERE sh.tripId = @tripId
), startTimes AS (
    SELECT tripStartDate, MIN(shouldDepartAt) AS start
    FROM stopHist
    GROUP BY tripStartDate
), tripStops AS (
    SELECT * FROM
    (SELECT DISTINCT ON (tripStopIndex)
        tripStopIndex,
        EXTRACT(EPOCH FROM shouldArriveAt
                - (SELECT start FROM startTimes AS sts
                   WHERE sts.tripStartDate = sh.tripstartdate))
            -- Guard against two+ stops in a row having the same x
            + (SELECT COUNT(*) * 10 FROM stopHist AS sh2
               WHERE sh2.tripStopIndex < sh.tripStopIndex
                 AND sh2.shouldArriveAt = sh.shouldArriveAt) AS x,
        s.name || ' (' || to_char(shouldArriveAt, 'HH24:MI') || ')' AS label,
        EXTRACT(EPOCH FROM shouldDepartAt - shouldArriveAt) AS dwellTime,
        TRUE AS arrival,
        FALSE AS departure
    FROM stopHist AS sh
    LEFT JOIN startTimes AS sts ON sts.tripStartDate = sh.tripStartDate
    LEFT JOIN stops AS s ON s.id = stopId
    UNION ALL
    SELECT DISTINCT ON (stopId)
        tripStopIndex,
        EXTRACT(EPOCH FROM CASE
            WHEN shouldDepartAt = shouldArriveAt THEN
                shouldDepartAt -
                (SELECT start FROM startTimes AS sts
                 WHERE sts.tripStartDate = sh.tripstartdate)
                + '1sec'::INTERVAL
            ELSE shouldDepartAt -
                (SELECT start FROM startTimes AS sts
                 WHERE sts.tripStartDate = sh.tripstartdate)
        END) + (SELECT COUNT(*) * 10 FROM stopHist AS sh2
                WHERE sh2.tripStopIndex < sh.tripStopIndex
                  AND sh2.shouldDepartAt = sh.shouldDepartAt) AS x,
        s.name || ' (' || to_char(shouldDepartAt, 'HH24:MI') || ')' AS label,
        NULL AS dwellTime,
        FALSE AS arrival,
        TRUE AS departure
    FROM stopHist AS sh
    LEFT JOIN startTimes AS sts ON sts.tripStartDate = sh.tripStartDate
    LEFT JOIN stops AS s ON s.id = stopId) AS i
    WHERE x IS NOT NULL
    ORDER BY x
), delays AS (
    SELECT
        tripStartDate,
        x,
        CASE
            WHEN arrival THEN arrivalDelay
            WHEN departure THEN departureDelay
        END AS delay
    FROM tripStops AS s
    LEFT JOIN stopHist AS sh ON sh.tripStopIndex = s.tripStopIndex
)
SELECT
    (SELECT json_agg(delays)
     FROM (SELECT json_agg(delay ORDER BY x) AS delays
           FROM delays
           GROUP BY tripStartDate) AS i) AS delays,
    (SELECT json_agg(json_build_object(
        'x', x,
        'label', label,
        'dwellTime', dwellTime,
        'arrival', arrival,
        'departure', departure))
     FROM tripStops) AS stops;
            """ ["tripId", box tripId
                 "tripStartDate", box ssgDate
                 "fromDate", box fromDate
                 "toDate", box toDate]
            |> Seq.exactlyOne
        let stops = JsonSerializer.Deserialize<TripDelayStop array>
                        (out.["stops"] :?> string)
        let delays = JsonSerializer.Deserialize<float option array array>
                        (out.["delays"] :?> string)
        stops, delays

    let stopsTable (stops: WebTripStopAgg array) =
        table [] [
            thead [] [
                th [] [str "Stop"]
                th [] [str "Exp. arr."]
                th [] [str "15th% arr. delay"]
                th [] [str "Median arr. delay"]
                th [] [str "85th% arr. delay"]
                th [] [str "Exp. dep."]
                th [] [str "15th% dep. delay"]
                th [] [str "Median dep. delay"]
                th [] [str "85th% dep. delay"]
            ]
            tbody [] [
                for stop in stops do
                let delay d =
                    d
                    |> Option.map (sprintf "%0.1f")
                    |> Option.defaultValue ""
                    |> str
                tr [] [
                    td [] [str stop.stopName]
                    td [] [
                        str <| Option.defaultValue "" stop.shouldArriveAt]
                    td [] [delay stop.p15ArrivalDelay ]
                    td [] [delay stop.medArrivalDelay]
                    td [] [delay stop.p85ArrivalDelay]
                    td [] [
                        str <| Option.defaultValue "" stop.shouldDepartAt]
                    td [] [delay stop.p15DepartureDelay]
                    td [] [delay stop.medDepartureDelay]
                    td [] [delay stop.p85DepartureDelay]
                ]
            ]
        ]

    let generateHeatmap (stops: TripDelayStop array)
                        (delays: float option array array) =
        let xRes = 120.0
        let yRes = 60.0
        let color = Pixel(0uy, 0uy, 255uy)

        let xs = stops |> Array.map (fun t -> t.x)
        let ys = delays |> Array.collect id |> Array.choose id
        let xMin = (xs |> Array.min |> floor) / xRes |> int
        let xMax = (xs |> Array.max |> ceil) / xRes |> int
        let yMin = (ys |> Array.min |> floor) / yRes |> int
        let yMax = (ys |> Array.max |> ceil) / yRes |> int
        // Convert from time and delay to image x,y
        let proj x delay =
            ((x / xRes |> int) - xMin), ((delay / yRes |> int) - yMin)

        let bitmap = Array2D.init (xMax - xMin + 1)
                                  (yMax - yMin + 1)
                                  (fun _ _ -> HashSet())
        let mark tripIdx x y () =
            bitmap.[x, y].Add(tripIdx) |> ignore

        for tripIdx, trip in delays |> Seq.indexed do
            let addStop stopIdx delay () =
                let x, y = proj stops.[stopIdx].x delay
                mark tripIdx x y ()
            let addLineBetweenStops stopIdx1 delay1 stopIdx2 delay2 () =
                let x1, y1 = proj stops.[stopIdx1].x delay1
                let x2, y2 = proj stops.[stopIdx2].x delay2
                // Bresenham's algorithm
                let dx = x2 - x1
                let dy = y2 - y1
                // We know x1 <= x2, so the algorithm can be a little simpler
                assert (dx >= 0)
                if abs dy < abs dx then
                    let yInc, dy = if dy >= 0 then 1, dy else -1, -dy
                    let mutable d = 2*dy - dx
                    let mutable y = y1
                    for x in x1..x2 do
                        mark tripIdx x y ()
                        if d > 0 then
                            y <- y + yInc
                            d <- d - 2*dx
                        d <- d + 2*dy
                else
                    let x1, y1, x2, y2, dx, dy =
                        if y1 < y2
                        then x1, y1, x2, y2, dx, dy
                        else x2, y2, x1, y1, x1 - x2, y1 - y2
                    let xInc, dx = if dx >= 0 then 1, dx else -1, -dx
                    let mutable d = 2*dx - dy
                    let mutable x = x1
                    for y in y1..y2 do
                        mark tripIdx x y ()
                        if d > 0 then
                            x <- x + xInc
                            d <- d - 2*dy
                        d <- d + 2*dx

            let tripSomes =
                trip
                |> Seq.indexed
                |> Seq.choose (fun (s, dopt) ->
                    dopt |> Option.map (fun d -> s, d))
                |> Seq.cache
            for ((prevStop, prevDelay), (stop, delay))
                    in tripSomes |> Seq.pairwise do
                addLineBetweenStops prevStop prevDelay stop delay ()
            let lastStop, lastDelay = tripSomes |> Seq.last
            addStop lastStop lastDelay ()

        let maxCount =
            seq {
                for x in 0..bitmap.GetLength(0) - 1 do
                    for y in 0..bitmap.GetLength(1) - 1 do
                        yield bitmap.[x, y].Count
            } |> Seq.max

        let png = PngBuilder.Create(
            bitmap.GetLength(0), bitmap.GetLength(1), true)
        for x in 0..bitmap.GetLength(0) - 1 do
            for y in 0..bitmap.GetLength(1) - 1 do
                let yInv = bitmap.GetLength(1) - y - 1
                let relCount = float bitmap.[x, yInv].Count / float maxCount
                let alpha = int <| 255.0 * Math.Sin(relCount * Math.PI / 2.0)
                png.SetPixel(
                    Pixel(color.R, color.G, color.B, byte alpha, false), x, y)
                |> ignore
        let memStream = new MemoryStream()
        png.Save(memStream)
        memStream.Seek(0, SeekOrigin.Begin) |> ignore
        memStream

    member this.get(
            tripId: string,
            fromDate: LocalDate,
            toDate: LocalDate) =
        let fromDate, toDate = dateRangeOrDefaults fromDate toDate
        let stopSeqGroups =
            getStopSeqGroups tripId fromDate toDate ()
            |> Array.map (fun ssg ->
                ssg,
                getTripName tripId ssg.date (),
                getTripStops tripId ssg.date fromDate toDate ())
        let tripName = getTripName tripId fromDate ()

        div [_class "trips-page"] [
            yield form [_class "controls"; _method "GET"] [
                dateRangeControl fromDate toDate
                label [_class "chart-type"] [
                    str "Chart type: "
                    select [] [
                        option [_value "lineAndBounds"] [
                            str "Median and percentile bounds"
                        ]
                        option [_value "heatmap"] [str "Heatmap"]
                    ]
                ]
                label [_class "show-table"] [
                    input [_type "checkbox"; _checked]
                    span [] [str " Show table"]
                ]
            ]

            for ssg, tripName, stops in stopSeqGroups do
                yield h1 [] [str $"Trip {tripName} (SSG {dateToIso ssg.date})"]
                yield details [_class "ssg-trips"] [
                    summary [] [str "Trips in SSG"]
                    ul [] [
                        for date in ssg.dates ->
                        let link = Links.trip tripId date
                        li [] [
                            a [_href link] [str <| dateToIso date]
                        ]
                    ]
                ]

                yield stopsTable stops

                yield div [
                    _class "delay-chart"
                    attr "data-ssg-date" (dateToIso ssg.date)] [
                        canvas [] []
                ]

                yield img [
                    _class "delay-heatmap"
                    _style "display: none"
                    _src "data:,"
                    attr "data-src"
                         ($"/Trips/{tripId}/heatmap"
                          + $"?fromDate={dateToIso fromDate}"
                          + $"&toDate={dateToIso toDate}"
                          + $"&ssgDate={dateToIso ssg.date}")
                ]
        ]
        |> htmlResult $"Trip {tripName}"

    [<HttpGet("chartData")>]
    member this.chartData(
            tripId: string,
            fromDate: LocalDate,
            toDate: LocalDate,
            ssgDate: LocalDate) =
        let fromDate, toDate = dateRangeOrDefaults fromDate toDate
        this.Content(
            getTripDelayChart tripId ssgDate fromDate toDate (),
            "application/json")

    [<HttpGet("heatmap")>]
    member this.heatmap(
            tripId: string,
            fromDate: LocalDate,
            toDate: LocalDate,
            ssgDate: LocalDate) =
        let fromDate, toDate = dateRangeOrDefaults fromDate toDate
        let stops, delays = getTripDelays tripId ssgDate fromDate toDate ()
        this.File(
            generateHeatmap stops delays,
            "image/png")
