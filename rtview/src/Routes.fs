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
        firstStop: string
        lastStop: string
    }

    [<Remote>]
    let routes (startDateBound: string * string) () =
        // TODO: Fully async?
        use c = getDbConn ()
        let routes =
            sqlQueryRec<WebRoute> c """
                SELECT DISTINCT ON (routeId)
                       routeId,
                       COALESCE(routeShortName, routeId) AS name,
                       (SELECT stopName FROM stophistorywithnames AS sh
                        WHERE sh.tripId = td.tripId
                          AND sh.tripstartdate = td.tripstartdate
                          ORDER BY tripStopIndex LIMIT 1) AS firstStop,
                       (SELECT stopName FROM stophistorywithnames AS sh
                        WHERE sh.tripId = td.tripId
                          AND sh.tripstartdate = td.tripstartdate
                          ORDER BY tripStopIndex DESC LIMIT 1) AS lastStop
                FROM tripDetails AS td
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
        use c = getDbConn ()
        let tripIds =
            sqlQueryRec<WebTrip> c """
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

        let tripDaysHtml (trips: Server.WebTrip array) =
            ul [attr.``class`` "days-list"] [
                for t in trips ->
                let link =
                    router.Link <| Locations.Trip (t.tripId, t.tripStartDate)
                let caption = sprintf "On %s" t.tripStartDate
                li [] [a [attr.href link] [text caption]]
            ]

        let tripsHtml (trips: Server.WebTrip array) =
            match trips |> Array.groupBy (fun t -> t.tripId) with
            | [| (_, ts) |] -> tripDaysHtml ts
            | tripsById ->
                ul [attr.``class`` "trips-list"]
                    (tripsById
                    |> Array.map (fun (tripId, ts) ->
                        let firstTrip = ts |> Array.head
                        let caption =
                            sprintf "%s" firstTrip.shortName
                        let expanded = Var.Create(false)

                        li [] [
                            span [attr.``class`` "trips-heading"
                                  on.click (fun _ _ ->
                                    expanded.Value <- not expanded.Value)]
                                 [text caption]
                            expanded.View.Doc (fun e ->
                                if e then tripDaysHtml ts
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
                     [span [attr.``class`` "route-name"] [text r.name]
                      span
                        [attr.``class`` "route-stops"]
                        [text (sprintf "(%s -> %s)" r.firstStop r.lastStop)]]
                tripsByRoute.View.Doc (fun tripsMap ->
                    tripsMap
                    |> Map.tryFind r.routeId
                    |> Option.map tripsHtml
                    |> Option.defaultValue Doc.Empty
                )
            ]

        div [attr.``class`` "routes-page"] [
            div [attr.``class`` "controls"] [
                label [] [
                    text "From:"
                    Doc.Input [attr.``type`` "date"] fromDate
                ]
                label [] [
                    text "To:"
                    Doc.Input [attr.``type`` "date"] toDate
                ]
            ]
            h2 [] [text "Routes"]
            ul [attr.``class`` "route-list"] [routes.DocSeqCached routeHtml]
        ]
