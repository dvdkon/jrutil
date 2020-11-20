// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module RtView.Utils

open WebSharper
open WebSharper.UI.Html
open WebSharper.ECharts

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
