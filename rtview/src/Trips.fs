// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2021 David Koňařík

module RtView.Trips

open System.Collections.Generic
open WebSharper
open NodaTime

open RtView.Utils

module Server =
    open RtView.ServerGlobals
    open JrUtil.SqlRecordStore

    [<JavaScript>]
    type StopSeqGroup = {
        date: string // Acts as an ID
        dates: string array
    }

    // Trips are divided into groups by their stop sequence, signified by the
    // tripStartDate of any trip of the group
    [<Remote>]
    let stopSeqGroups (tripId: string) (fromDate: string) (toDate: string) () =
        use c = getDbConn ()
        let tsDates =
            sqlQueryRec<StopSeqGroup> c """
-- TODO: Less nested query
SELECT date::text AS date, dates::text[] AS dates
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
        async { return tsDates }

    [<JavaScript>]
    type WebTripStop = {
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

    [<Remote>]
    let tripStops (tripId: string)
                  (tripStartDate: string)
                  (fromDate: string) (toDate: string)
                  () =
        use c = getDbConn ()
        let stops =
            sqlQueryRec<WebTripStop> c """
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
        async { return stops }

    [<JavaScript>]
    type DelayChartAvgItem = {
        x: float
        p15Delay: float
        delay: float
        p85Delay: float
        stopName: string
    }

    [<JavaScript>]
    type DelayChartLabel = {
        x: float
        label: string
    }

    [<JavaScript>]
    type DelayChartData = {
        data: DelayChartAvgItem array
        labels: DelayChartLabel array
    }

    [<Remote>]
    let tripDelayChart (tripId: string)
                       (tripStartDate: string)
                       (fromDate: string) (toDate: string)
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
        'delay', delay,
        'p85Delay', p85Delay,
        'stopName', stopName))
     FROM data) AS data,
    (SELECT json_agg(json_build_object(
        'x', x, 'label', label))
     FROM labels) AS labels;
            """ ["tripId", box tripId
                 "tripStartDate", box tripStartDate
                 "fromDate", box fromDate
                 "toDate", box toDate]
            |> Seq.exactlyOne
        let data =
            Json.Deserialize<DelayChartAvgItem array> (out.["data"] :?> string)
        let labels =
            Json.Deserialize<DelayChartLabel array> (out.["labels"] :?> string)
        async { return { data = data; labels = labels } }

    [<JavaScript>]
    type TripDelayStop = {
        x: float
        label: string option
        dwellTime: float option
        arrival: bool
        departure: bool
    }

    [<Remote>]
    let tripDelays (tripId: string)
                   (tripStartDate: string)
                   (fromDate: string) (toDate: string)
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
                 "tripStartDate", box tripStartDate
                 "fromDate", box fromDate
                 "toDate", box toDate]
            |> Seq.exactlyOne
        let stops = Json.Deserialize<TripDelayStop array> (out.["stops"] :?> string)
        let delays = Json.Deserialize<float array array> (out.["delays"] :?> string)
        async { return (stops, delays) }

[<JavaScript>]
module Client =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Client
    open WebSharper.ECharts
    open RtView.ClientGlobals

    type ChartType = LineAndBounds | Heatmap

    let showTable = Var.Create(true)
    let chartType = Var.Create(LineAndBounds)

    let tripsPage tripId fromDateParam toDateParam () =
        let fromDate, toDate = dateRangeOrDefaults fromDateParam toDateParam

        let stopsTable (stops: Server.WebTripStop array) =
            table [] [
                thead [] [
                    th [] [text "Stop"]
                    th [] [text "Exp. arr."]
                    th [] [text "15th% arr. delay"]
                    th [] [text "Median arr. delay"]
                    th [] [text "85th% arr. delay"]
                    th [] [text "Exp. dep."]
                    th [] [text "15th% dep. delay"]
                    th [] [text "Median dep. delay"]
                    th [] [text "85th% dep. delay"]
                ]
                tbody [] [
                    for stop in stops do
                    let delay d =
                        d
                        |> Option.map (sprintf "%0.1f")
                        |> Option.defaultValue ""
                        |> text
                    tr [] [
                        td [] [text stop.stopName]
                        td [] [
                            text <| Option.defaultValue "" stop.shouldArriveAt]
                        td [] [delay stop.p15ArrivalDelay ]
                        td [] [delay stop.medArrivalDelay]
                        td [] [delay stop.p85ArrivalDelay]
                        td [] [
                            text <| Option.defaultValue "" stop.shouldDepartAt]
                        td [] [delay stop.p15DepartureDelay]
                        td [] [delay stop.medDepartureDelay]
                        td [] [delay stop.p85DepartureDelay]
                    ]
                ]
            ]

        let lineChartOpts (data: Server.DelayChartData) =
            let seriesData =
                data.data |> Array.map (fun i -> [|
                    // Having to cast to float is actually a bug in WS
                    // A param (X|Y)[] gets compiled to X[] and Y[]. We also
                    // need obj[]
                    i.x
                    i.delay
                    box i :?> float
                |])
            let ticks =
                data.labels |> Array.map (fun l -> l.x)
            let labelMap =
                data.labels |> Array.map (fun l -> l.x, l.label) |> Map
            let minY =
                data.data |> Array.map (fun i -> i.p15Delay) |> Array.min
            let botY = if minY < -5. then floor (minY - 1.) else -5.
            let maxY =
                data.data |> Array.map (fun i -> i.p85Delay) |> Array.max
            let topY = if maxY > 10. then ceil (maxY + 1.) else 10.

            let bandRenderer (pars: SeriesCustom_RenderItemParams,
                              api: SeriesCustom_RenderItemApi) =
                let i = int pars.DataIndex.Value
                let j = i + 1
                let points =
                    if j >= data.data.Length
                    then [||]
                    else [|
                            [| data.data.[i].x; data.data.[i].p85Delay |]
                            [| data.data.[i].x; data.data.[i].p15Delay |]
                            [| data.data.[j].x; data.data.[j].p15Delay |]
                            [| data.data.[j].x; data.data.[j].p85Delay |]
                        // TODO: Give api.coord the proper type
                        |] |> Array.map (api.Coord :> obj :?> float array -> float array)
                SeriesCustom_RenderItemReturnPolygon()
                 .SetType("polygon")
                 .SetShape(
                   SeriesCustom_RenderItemReturnPolygon_shape(points :> obj :?> obj array))
                 .SetStyle(
                   SeriesCustom_RenderItemReturnPolygon_style()
                    .SetFill("#aaa5")
                 )

            EChartOption()
             .SetGrid([|delayChartGrid ()|])
             .SetXAxis(delayChartXAxis ticks labelMap)
             .SetYAxis(
                YAxis()
                 .SetType("value")
                 .SetMin(fun x -> botY)
                 .SetMax(fun x -> topY)
                 .SetInterval(5.) // TODO: Sometimes doesn't work
             )
             .SetTooltip(
                Tooltip()
                 .SetShow(true)
                 .SetTrigger("axis")
                 .SetFormatter(fun (pars: Tooltip_Format array,
                                    ticket: string,
                                    callback: JavaScript.FuncWithArgs<
                                        (string * string),Unit>) ->
                    let data = pars.[0].Data.Value :?> obj array
                    let item = data.[2] :?> Server.DelayChartAvgItem
                    sprintf "%s<br>85th%% delay: %.1f min<br>Median delay: %.1f min<br>15th%% delay: %.1f"
                            item.stopName item.p85Delay item.delay
                            item.p15Delay
                 )
             )
             .SetAxisPointer(
                AxisPointer()
                 .SetSnap(true)
             )
             .SetSeries([|
                SeriesCustom()
                 .SetType("custom")
                 .SetData(data.data :> obj :?> SeriesCustom_DataObject array)
                 .SetRenderItem(bandRenderer)
                 // TODO: Fix the bindings so this isn't needed!
                 // This is because internally interface inheritance is
                 // flattened due to "duplicate method" problems
                 :> obj :?> SeriesLine
                SeriesLine()
                 .SetType("line")
                 .SetData(seriesData)
                 .SetShowSymbol(false)
             |])
             .SetVisualMap([|delayChartVisualMap ()|])

        let heatmapChartOpts xRes yRes (stops: Server.TripDelayStop array) (delays: float array array) =
            let xs = stops |> Array.map (fun t -> t.x)
            let ys = delays |> Array.collect id
            let xMin = (xs |> Array.min |> floor) / xRes |> int
            let xMax = (xs |> Array.max |> ceil) / xRes |> int
            let yMin = (ys |> Array.min |> floor) / yRes |> int
            let yMax = (ys |> Array.max |> ceil) / yRes |> int

            let bitmap = Array2D.init (xMax - xMin + 1) (yMax - yMin + 1) (fun _ _ -> JavaScript.Array())
            let mark tripIdx x y () =
                let tripArr = bitmap.[x - xMin, y - yMin]
                if tripArr.IndexOf(tripIdx) = -1 then tripArr.Push(tripIdx) |> ignore

            for tripIdx, trip in delays |> Seq.indexed do
                let addStop stopIdx delay () =
                    mark tripIdx (stops.[stopIdx].x / xRes |> int) (delay / yRes |> int) ()
                let addLineBetweenStops stopIdx1 delay1 stopIdx2 delay2 () =
                    let x1 = stops.[stopIdx1].x
                    let x2 = stops.[stopIdx2].x
                    let slope = (delay2 - delay1) / (x2 - x1)
                    for x in seq {0. .. ((x2 - x1) / xRes)} do
                        let y = (((slope * x * xRes) / yRes + delay1) / yRes) |> int
                        mark tripIdx ((x + x1/xRes) |> int) y ()

                for ((prevStop, prevDelay), (stop, delay)) in trip |> Seq.indexed |> Seq.pairwise do
                    addLineBetweenStops prevStop prevDelay stop delay ()
                addStop (trip.Length - 1) trip.[trip.Length - 1] ()

            let heatmapPoints =
                Seq.allPairs (seq {0..(bitmap |> Array2D.length1)-1})
                             (seq {0..(bitmap |> Array2D.length2)-1})
                |> Seq.map (fun (x, y) -> [| float x; float y; bitmap.[x, y].Length |> float |])
                |> Seq.filter (fun a -> a.[2] <> 0.)
                |> Seq.toArray

            let labelMap =
                stops
                |> Array.sortByDescending (fun s -> s.dwellTime)
                |> Array.take 10
                |> Array.map (fun s -> s.x / xRes |> int, s.label)
                |> Array.groupBy (fun (x, _) -> x)
                |> Array.map (fun (x, ns) -> (x, Array.head ns |> snd))
                |> Array.append [|
                    xMin, stops.[0].label
                    xMax, stops.[stops.Length-1].label
                |]
                |> Map

            EChartOption()
             .SetGrid([|delayChartGrid ()|])
             .SetXAxis(
                XAxis()
                 .SetType("category")
                 .SetData([| xMin..xMax |] |> Array.map float)
                 .SetAxisLabel(
                    CartesianAxis_Label()
                     .SetInterval(fun (i, v) -> labelMap |> Map.containsKey (int v))
                     .SetFormatter(fun (v, _) ->
                        labelMap
                        |> Map.tryFind (int v)
                        |> Option.bind id
                        |> Option.defaultValue (string v)
                     )
                     .SetRotate(80.)
                 )
                 .SetAxisTick(
                    CartesianAxis_Tick()
                     .SetInterval(fun (i, v) -> labelMap |> Map.containsKey (int v))
                     .SetAlignWithLabel(true)
                 )
             )
             .SetYAxis(
                YAxis()
                 .SetType("category")
                 .SetData([| yMin..yMax |] |> Array.map (fun y -> float y * yRes / 60.))
                 .SetAxisTick(
                    CartesianAxis_Tick()
                     .SetAlignWithLabel(true)
                     .SetInterval(fun i v -> (int v) % 5 = 0)
                 )
                 .SetAxisLabel(
                    CartesianAxis_Label()
                     .SetInterval(fun i v -> (int v) % 5 = 0)
                 )
             )
             .SetSeries([|
                SeriesHeatmap()
                 .SetType("heatmap")
                 .SetData(heatmapPoints)
             |])
             .SetVisualMap([|
                VisualMap_Continuous()
                 .SetMin(1.)
                 .SetMax(float delays.Length) // Trip count
                 .SetInRange(
                    VisualMap_RangeObject()
                     .SetColor([|"#99e"|])
                     .SetOpacity([|1. / float delays.Length; 1.|])
                 )
                 .SetShow(false)
             |])

        let stopSeqGroups = Var.Create([||])
        async {
            let! ssgs =
                asyncWithLoading "Loading stop sequence group list" <|
                    Server.stopSeqGroups tripId fromDate toDate ()
            stopSeqGroups.Value <- ssgs
        } |> Async.Start

        div [attr.``class`` "trips-page"] [
            dateRangeControl fromDate toDate (fun f t () ->
                setLocation <| Locations.Trips (tripId, f, t)
            )
            div [] [label [] [
                Doc.CheckBox [] showTable
                text "Show table"
            ]]
            div [] [label [] [
                text "Chart type: "
                Doc.Select
                    []
                    (fun k -> Map.find k <| Map [
                        LineAndBounds, "Median and percentile bounds"
                        Heatmap, "Heatmap"
                    ])
                    [LineAndBounds; Heatmap]
                    chartType
            ]]

            stopSeqGroups.View.DocSeqCached (fun (ssg: Server.StopSeqGroup) ->
                let tripName = Var.Create("")
                async {
                    let! tn = getTripName tripId ssg.date ()
                    tripName.Value <- tn
                } |> Async.Start

                let stops =
                    showTable.View.MapAsyncLoading
                        [||]
                        (fun show ->
                            if not show then async { return [||] }
                            else async {
                                let! s =
                                    asyncWithLoading "Loading trip stop list..." <|
                                        Server.tripStops tripId ssg.date fromDate toDate ()
                                return s
                            })

                div [] [
                    tripName.View.Doc (fun tn ->
                        h1 [] [text (sprintf "Trip %s (SSG %s)" tn ssg.date)])
                    details [attr.``class`` "ssg-trips"] [
                        summary [] [text "Trips in SSG"]
                        ul [] [
                            for date in ssg.dates ->
                            let link = router.Link <| Locations.Trip (tripId, date)
                            li [] [
                                a [attr.href link] [text date]
                            ]
                        ]
                    ]

                    stops.Doc (fun ss -> if ss <> [||] then stopsTable ss else Doc.Empty)

                    chartType.View.Doc (fun ct ->
                        createChart "delay-chart" (fun c ->
                            async {
                                match ct with
                                | LineAndBounds ->
                                    let! data =
                                        asyncWithLoading "Loading trip delay chart..." <|
                                            Server.tripDelayChart tripId ssg.date fromDate toDate ()
                                    return lineChartOpts data
                                | Heatmap ->
                                    let! (stops, delays) =
                                        asyncWithLoading "Loading trip delay heatmap..." <|
                                            Server.tripDelays tripId ssg.date fromDate toDate ()
                                    return heatmapChartOpts 240. 60. stops delays
                            }
                        )
                    )
                ]
            )
        ]
