// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module RtView.Utils

open WebSharper
open WebSharper.UI.Html
open WebSharper.ECharts

open JrUtil.SqlRecordStore
open RtView.ServerGlobals

[<Remote>]
let getTripName (tripId: string) (startDate: string) () =
    use c = getDbConn ()
    let name = sqlQueryOne c """
            SELECT COALESCE(shortName, @tripId)
            FROM tripDetails
            WHERE tripId = @tripId
              AND tripStartDate = @startDate::date
            LIMIT 1
            """ ["tripId", box tripId
                 "startDate", box startDate]
    async { return name :?> string }

[<JavaScript>]
let createChart (cssCls: string) (optsGetter: ECharts -> Async<EChartOption>) =
    div [
        attr.``class`` cssCls
        on.afterRender (fun el ->
            let chart = Echarts.Init(el :?> JavaScript.HTMLElement)
            JavaScript.JS.Window.AddEventListener("resize", fun () ->
                chart.Resize())
            async {
                let! opts = optsGetter chart
                chart.SetOption(opts)
            } |> Async.Start
        )
    ] []

[<JavaScript>]
let delayChartXAxis (ticks: float array) (labelMap: Map<float, string>) =
    XAxis()
     .SetType("value")
     .SetAxisLabel(
        CartesianAxis_Label()
         .SetCustomValues(ticks)
         .SetFormatter(fun (v, _) ->
            // TODO: Why do I get invalid keys?
            labelMap
            |> Map.tryFind (float v)
            |> Option.defaultValue v
         )
         .SetRotate(80.)
     )
     .SetAxisTick(
        CartesianAxis_Tick()
         .SetCustomValues(ticks)
         .SetAlignWithLabel(true)
     )
     .SetMin(fun x -> ticks |> Array.min)
     .SetMax(fun x -> ticks |> Array.max)

[<JavaScript>]
let delayChartVisualMap () =
    VisualMap_Piecewise()
     .SetType("piecewise")
     .SetShow(false)
     .SetDimension(1.)
     .SetPieces([|
        // +.2 to avoid problems with lines on boundary
        VisualMap_PiecesObject()
         .SetMax(-0.2)
         .SetColor("#1ae0d3")
        VisualMap_PiecesObject()
         .SetMin(-0.2)
         .SetMax(5.2)
         .SetColor("#1ae070")
        VisualMap_PiecesObject()
         .SetMin(5.2)
         .SetMax(10.2)
         .SetColor("#e0d31a")
        VisualMap_PiecesObject()
         .SetMin(10.2)
         .SetColor("#e01a28")
     |])