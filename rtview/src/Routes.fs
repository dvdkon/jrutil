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

    [<JavaScript>]
    type WebRoutesResp = {
        fullRouteCount: int64
        routes: WebRoute array
    }

    [<JavaScript>]
    let routesPerPage = 25

    [<Remote>]
    let routes (startDateBound: string * string) 
               (page: int)
               (searchTerm: string option)() =
        // TODO: Fully async?
        use c = getDbConn ()
        let routesQuery = """
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
              -- TODO: Full text search also on the stop names
              AND routeShortName ILIKE @searchLike
        """
        let pars = ["dateFrom", box <| fst startDateBound
                    "dateTo", box <| snd startDateBound
                    "searchLike",
                        box <| "%" + (Option.defaultValue "" searchTerm) + "%"
                    "page", box <| page - 1
                    "perPage", box routesPerPage]

        let routeCount =
            unbox<int64> <| sqlQueryOne c (
                "SELECT COUNT(routeId) FROM (" + routesQuery + ") AS i") pars
        let routes =
            sqlQueryRec<WebRoute> c (routesQuery + """
                ORDER BY routeId, routeShortName
                LIMIT @perPage
                OFFSET @page * @perPage
                """) pars 
            |> Seq.toArray
        async { return { fullRouteCount = routeCount; routes = routes } }

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

    // Some Vars are global in the module to persist when navigating without having to add GET params
    let page = Var.Create(1)

    let tripList fromDateParam toDateParam searchParam () =
        let today () = JavaScript.Date().ToISOString().Split([|'T'|]).[0]
        let fromDate = fromDateParam |> Option.defaultWith today
        let toDate = toDateParam |> Option.defaultWith today
        // TODO: A var per route or this?
        let tripsByRoute = Var.Create(Map<string, Server.WebTrip array> [])

        let searchTerm =
            Var.Create (searchParam |> Option.defaultValue "")

        let routesRes =
            page.View.MapAsync (fun page ->
                Server.routes (fromDate, toDate) page searchParam ())
        let fullRouteCount = routesRes.Map (fun res -> res.fullRouteCount)
        let routes = routesRes.Map (fun res -> res.routes)

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
                        Server.trips (fromDate, toDate)
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
                div [attr.``class`` "date-range"] [
                    label [] [
                        text "From:"
                        input [attr.``type`` "date"
                               attr.value fromDate
                               on.change (fun el _ ->
                                   page.Value <- 1
                                   setLocation <| Locations.Routes (
                                       BindVar.StringGet(el),
                                       Some toDate,
                                       searchParam)
                               )] []
                    ]
                    label [] [
                        text "To:"
                        input [attr.``type`` "date"
                               attr.value toDate
                               on.change (fun el _ ->
                                   page.Value <- 1
                                   setLocation <| Locations.Routes (
                                       Some fromDate,
                                       BindVar.StringGet(el),
                                       searchParam)
                              )] []
                    ]
                ]
                form [attr.``class`` "search"
                      on.submit (fun _ ev ->
                          ev.PreventDefault()
                          setLocation <| Locations.Routes (
                              Some fromDate,
                                  Some toDate,
                                  let v = searchTerm.Value
                                  if v.Trim() = "" then None else Some v))] [
                    Doc.Input [attr.placeholder "Search term"] searchTerm
                    button [] [text "Search"]
                ]
            ]
            h2 [] [text "Routes"]
            ul [attr.``class`` "route-list"] [routes.DocSeqCached routeHtml]
            V(page.V, fullRouteCount.V).Doc (fun (p, rc) ->
                div [attr.``class`` "pagination"] [
                    let pageCount =
                        (float rc) / (float Server.routesPerPage)
                        |> ceil |> int
                    yield a
                        [attr.``class`` "back"
                         attr.style (if p = 1 then "visibility: hidden"
                                     else "")
                         on.click (fun _ _ -> page.Value <- p - 1)]
                        [text "Back"]
                    yield span [] [
                        text <| if pageCount = 0 then "No routes"
                                else sprintf "Page %d/%d" p pageCount]
                    yield a
                        [attr.``class`` "forward"
                         attr.style (if p >= pageCount
                                     then "visibility: hidden" else "")
                         on.click (fun _ _ -> page.Value <- p + 1)]
                        [text "Forward"]
                ]
            )
        ]
