// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2020 David Koňařík

module RtView.Routes

open WebSharper

module Server =
    open RtView.ServerGlobals
    open JrUtil.SqlRecordStore

    [<JavaScript>]
    type WebRoute = {
        routeId: string
        name: string
    }

    [<Remote>]
    let routes (startDateBound: string * string) () =
        // TODO: Fully async?
        let routes =
            sqlQueryRec<WebRoute> dbConn.Value """
                SELECT DISTINCT ON (routeId)
                       routeId,
                       COALESCE(routeShortName, routeId) AS name
                FROM tripDetails
                WHERE tripStartDate >= @dateFrom::date
                  AND tripStartDate <= @dateTo::date
                """ ["dateFrom", box <| fst startDateBound
                     "dateTo", box <| snd startDateBound]
            |> Seq.toArray
        async { return routes }

    [<JavaScript>]
    type WebTrip = {
        tripId: string
        tripStartDate: string
        shortName: string
    }

    [<Remote>]
    let trips (startDateBound: string * string) (routeId: string) () =
        // TODO: Fully async?
        let tripIds =
            sqlQueryRec<WebTrip> dbConn.Value """
                SELECT tripId, tripStartDate::text,
                       COALESCE(shortName, tripId) AS shortName
                FROM tripDetails
                WHERE routeId = @routeId
                  AND tripStartDate >= @dateFrom::date
                  AND tripStartDate <= @dateTo::date
                """ ["dateFrom", box <| fst startDateBound
                     "dateTo", box <| snd startDateBound
                     "routeId", box routeId]
            |> Seq.toArray
        async { return tripIds }

[<JavaScript>]
module Client =
    open WebSharper.UI
    open WebSharper.UI.Html
    open WebSharper.UI.Client

    open RtView.ClientGlobals

    let tripList fromDateParam toDateParam () =
        let today () = JavaScript.Date().ToISOString().Split([|'T'|]).[0]
        let fromDate = Var.Create(fromDateParam |> Option.defaultWith today)
        let toDate = Var.Create(toDateParam |> Option.defaultWith today)
        // TODO: A var per route or this?
        let tripsByRoute = Var.Create(Map<string, Server.WebTrip array> [])

        let routes =
            V(fromDate.V, toDate.V)
            |> View.MapAsync (fun dr ->
                setLocation <| Routes (Some <| fst dr, Some <| snd dr)
                Server.routes dr ())

        let tripsHtml (trips: Server.WebTrip array) =
            ul []
                (trips
                |> Array.groupBy (fun t -> t.tripId)
                |> Array.map (fun (tripId, ts) ->
                    let firstTrip = ts |> Array.head
                    let caption =
                        sprintf "%s" firstTrip.shortName
                    let expanded = Var.Create(false)

                    li [] [
                        span [attr.``class`` "trips-heading"
                              on.click (fun _ _ -> expanded.Value <- not expanded.Value)]
                             [text caption]
                        expanded.View.Doc (fun e ->
                            if e then
                                ul [] [
                                    for t in ts ->
                                    let link =
                                        router.Link
                                        <| Locations.Trip (t.tripId, t.tripStartDate)
                                    let caption = sprintf "On %s" t.tripStartDate
                                    li [] [a [attr.href link] [text caption]]
                                ]
                            else Doc.Empty
                        )
                    ]
                ))

        let onRouteClick (r: Server.WebRoute) () =
            async {
                if tripsByRoute.Value
                   |> Map.containsKey r.routeId
                   |> not then
                    let! trips =
                        Server.trips (fromDate.Value, toDate.Value)
                                     r.routeId ()
                    tripsByRoute.Value <- tripsByRoute.Value
                                          |> Map.add r.routeId trips
                else
                    tripsByRoute.Value <- tripsByRoute.Value
                                          |> Map.remove r.routeId
            } |> Async.Start

        let routeHtml (r: Server.WebRoute) =
            li [] [
                    span [attr.``class`` "route-heading"
                          on.click (fun _ _ -> onRouteClick r ())]
                         [text r.name]
                    tripsByRoute.View.Doc (fun tripsMap ->
                        tripsMap
                        |> Map.tryFind r.routeId
                        |> Option.map tripsHtml
                        |> Option.defaultValue Doc.Empty
                    )
            ]

        div [attr.``class`` "main"] [
            div [attr.``class`` "controls"] [
                text "From:"
                Doc.Input [attr.``type`` "date"] fromDate
                text "To:"
                Doc.Input [attr.``type`` "date"] toDate
            ]
            h2 [] [text "Routes"]
            ul [] [routes.DocSeqCached routeHtml]
        ]
