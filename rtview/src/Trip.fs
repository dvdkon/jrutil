// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module RtView.Trip

open WebSharper

module Server =
    open RtView.ServerGlobals
    open JrUtil.SqlRecordStore

    [<Remote>]
    let tripName (tripId: string) (startDate: string) () =
        let name =
            sqlQueryOne dbConn.Value """
                SELECT COALESCE(shortName, @tripId)
                FROM tripDetails
                WHERE tripId = @tripId
                  AND tripStartDate = @startDate::date
                """ ["tripId", box tripId
                     "startDate", box startDate]
        async { return name :?> string }

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
        let stops =
            sqlQueryRec<WebTripStop> dbConn.Value """
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

    [<Remote>]
    let tripDelayChart (tripId: string) (startDate: string) () =
        let items =
            sqlQueryRec<DelayChartItem> dbConn.Value """
SELECT * FROM (
    -- Arrival delays
    SELECT EXTRACT(EPOCH FROM shouldArriveAt - first_value(shouldDepartAt)
                                  OVER (ORDER BY tripStopIndex)) AS x,
           EXTRACT(EPOCH FROM arrivedAt - shouldArriveAt)/60 AS delay,
           stopName
    FROM stopHistoryWithNames
    WHERE tripId = @tripId
      AND tripStartDate = @startDate::date
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
        END) AS x,
        EXTRACT(EPOCH FROM departedAt - shouldDepartAt)/60 AS delay,
        stopName
    FROM stopHistoryWithNames
    WHERE tripId = @tripId
      AND tripStartDate = @startDate::date
) AS i
WHERE x IS NOT NULL AND delay IS NOT NULL
ORDER BY x;
                """ ["tripId", box tripId
                     "startDate", box startDate]
            |> Seq.toArray
        async { return items }

[<JavaScript>]
module Client =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Client
    open WebSharper.ChartJs

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
            let! n = Server.tripName tripId tripStartDate ()
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

        let createDelayChart (el: JavaScript.Dom.Element)
                             (data: Server.DelayChartItem array) =
            let dataset = LineChartDataSet()
            dataset.Label <- "Delay (minutes)"
            dataset.Data <-
                data
                |> Array.map (fun i -> {x = i.x; y = i.delay})
                |> box

            let chartData = ChartData([| dataset |])
            chartData.Labels <-
                data |> Array.map (fun i -> i.stopName)

            // TODO: Why isn't there a convenient constructor for this?
            let config = CommonChartConfig()
            (*config.Scale <- Scales()
            config.Scale.XAxes <- Scale()
            config.Scale.XAxes.Ticks <- TickConfig()
            config.Scale.XAxes.AfterBuildTicks <- fun _ ->
                data |> Array.map (fun i -> i.x)*)

            Chart(el, ChartCreate("scatter", chartData, config)) |> ignore

        let delayChartElem =
            canvas [on.afterRender (fun el ->
                async {
                    let! data = Server.tripDelayChart tripId tripStartDate ()
                    createDelayChart el data
                } |> Async.Start)] []

        div [] [
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
            div [] [delayChartElem]
        ]