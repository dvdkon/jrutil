// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module RtView.Trip

open WebSharper

open RtView.Utils

module Server =
    open RtView.ServerGlobals
    open JrUtil.SqlRecordStore

    [<JavaScript>]
    type WebTripStop = {
        stopId: string
        stopName: string
        arrivedAt: string option
        shouldArriveAt: string option
        arrivalDelay: string option
        departedAt: string option
        shouldDepartAt: string option
        departureDelay: string option
    }

    [<Remote>]
    let tripStops (tripId: string) (startDate: string) () =
        use c = getDbConn ()
        let stops =
            sqlQueryRec<WebTripStop> c """
                SELECT stopId, stopName,
                       to_char(arrivedAt, 'HH24:MI') AS arrivedAt,
                       to_char(shouldArriveAt, 'HH24:MI') AS shouldArriveAt,
                       (EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt)/60)::text
                           AS arrivalDelay,
                       to_char(departedAt, 'HH24:MI') AS departedAt,
                       to_char(shouldDepartAt, 'HH24:MI') as shouldDepartAt,
                       (EXTRACT(EPOCH FROM departedAt - shouldDepartAt)/60)::text
                           AS departureDelay
                FROM stopHistoryWithNames
                WHERE tripId = @tripId
                  AND tripStartDate = @startDate::date
                ORDER BY tripStopIndex
                """ ["tripId", box tripId
                     "startDate", box startDate]
            |> Seq.toArray
        async { return stops }

    [<JavaScript>]
    type DelayChartItem = {
        x: float
        delay: float
        stopName: string
    }

    [<JavaScript>]
    type DelayChartLabel = {
        x: float
        label: string
    }

    [<JavaScript>]
    type DelayChartData = {
        data: DelayChartItem array
        labels: DelayChartLabel array
    }

    [<Remote>]
    let tripDelayChart (tripId: string) (startDate: string) () =
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
        'x', x, 'delay', delay, 'stopName', stopName))
     FROM data) AS data,
    (SELECT json_agg(json_build_object(
        'x', x, 'label', label))
     FROM labels) AS labels;
                """ ["tripId", box tripId
                     "startDate", box startDate]
            |> Seq.exactlyOne
        let data =
            Json.Deserialize<DelayChartItem array> (out.["data"] :?> string)
        let labels =
            Json.Deserialize<DelayChartLabel array> (out.["labels"] :?> string)
        async { return { data = data; labels = labels } }

[<JavaScript>]
module Client =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Client
    open WebSharper.ECharts

    type ChartPoint = {
        x: float
        y: float
    }

    let tripPage tripId tripStartDate () =
        let stops = Var.Create([||])
        async {
            let! s = Server.tripStops tripId tripStartDate ()
            stops.Value <- s
        } |> Async.Start

        let tripName = Var.Create("")
        async {
            let! n = getTripName tripId tripStartDate ()
            tripName.Value <- n
        } |> Async.Start

        let stopRow (s: Server.WebTripStop) =
            tr [] [
                td [] [text s.stopName]
                td [] [text (s.shouldArriveAt |> Option.defaultValue "")]
                td [] [text (s.arrivedAt |> Option.defaultValue "")]
                td [] [text (s.arrivalDelay |> Option.defaultValue "")]
                td [] [text (s.shouldDepartAt |> Option.defaultValue "")]
                td [] [text (s.departedAt |> Option.defaultValue "")]
                td [] [text (s.departureDelay |> Option.defaultValue "")]
            ]

        let chartOpts (data: Server.DelayChartData) =
            let seriesData =
                data.data |> Array.map (fun i -> [|
                    // Having to cast to float is actually a bug in WS
                    // A param (X|Y)[] gets compiled to X[] and Y[]. We also
                    // need obj[]
                    i.x
                    i.delay
                    box i.stopName :?> float
                |])
            let ticks =
                data.labels |> Array.map (fun l -> l.x)
            let labelMap =
                data.labels |> Array.map (fun l -> l.x, l.label) |> Map

            EChartOption()
             .SetGrid([|delayChartGrid ()|])
             .SetXAxis(delayChartXAxis ticks labelMap)
             .SetYAxis(
                YAxis()
                 .SetType("value")
                 .SetMin(fun x -> if x.Min < -5. then ceil (x.Min - 1.) else -5.)
                 .SetMax(fun x -> if x.Max > 10. then ceil (x.Max + 1.) else 10.)
                 .SetInterval(5.)
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
                    sprintf "%s<br>Delay: %.0f min"
                            (data.[2] :?> string)
                            (data.[1] :?> float)
                 )
             )
             .SetAxisPointer(
                AxisPointer()
                 .SetSnap(true)
             )
             .SetSeries([|
                SeriesLine()
                 .SetType("line")
                 .SetData(seriesData)
                 .SetShowSymbol(false)
                 // Disabled since SmoothMonotone is broken with points too
                 // close on the X axis
                 .SetSmooth(false)
                 .SetSmoothMonotone("x")
             |])
             .SetVisualMap([|delayChartVisualMap ()|])

        div [attr.``class`` "trip-page"] [
            h1 [] [
                tripName.View.Doc (fun n ->
                    text (sprintf "Trip %s on %s" n tripStartDate))]
            table [] [
                thead [] [
                    th [] [text "Stop"]
                    th [] [text "Exp. arr."]
                    th [] [text "Arr."]
                    th [] [text "Arr. delay"]
                    th [] [text "Exp. dep."]
                    th [] [text "Dep."]
                    th [] [text "Dep. delay"]
                ]
                tbody [] [
                    stops.View.DocSeqCached stopRow
                ]
            ]
            createChart "delay-chart" (fun c ->
                async {
                    let! data = Server.tripDelayChart tripId tripStartDate ()
                    return chartOpts data
                }
            )
        ]
