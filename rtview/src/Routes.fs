// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

namespace RtView

open Microsoft.AspNetCore.Mvc
open Giraffe.ViewEngine
open NodaTime
open FsToolkit.ErrorHandling

open JrUtil.Utils
open JrUtil.SearchParser
open JrUtil.SqlSearch
open JrUtil.SqlRecordStore
open RtView.ServerGlobals
open RtView.Utils

#nowarn "40"

type WebRoute = {
    routeId: string
    name: string
    firstStop: string option
    lastStop: string option
}

type WebTrip = {
    tripId: string
    tripStartDate: LocalDate
    shortName: string
}

[<Route("/"); Route("/Routes")>]
type RoutesController() =
    inherit ControllerBase()

    let routesPerPage = 25

    let stopSearchFields = Map [
        "", stringField "name"
        "arr", timeField "arrivedAt::time"
        "dep", timeField "departedAt::time"
        "parr", timeField "shouldArriveAt::time"
        "pdep", timeField "shouldDepartAt::time"
    ]

    let rec searchFields = Map [
        "", stringField "name"
        "not", notField (fun () -> searchFields)
        "routename", stringField "name"
        "occurs", intField """
            (SELECT COUNT(*)
             FROM tripDetails AS td
             WHERE td.routeId = rs.routeId
               AND td.tripStartDate >= @dateFrom
               AND td.tripStartDate <= @dateTo)
        """
        "stop", fun pa args -> result {
            let! inner = innerArgsToSql stopSearchFields pa args
            return $"""
                EXISTS (
                    SELECT FROM tripDetails AS td
                    INNER JOIN stopHistory AS sh
                        ON sh.tripId = td.tripId
                       AND sh.tripStartDate = td.tripStartDate
                    INNER JOIN stops AS s
                        ON s.id = sh.stopid
                       AND s.validDateRange @> sh.tripStartDate
                    WHERE td.routeId = rs.routeId
                      AND td.tripStartDate >= @dateFrom
                      AND td.tripStartDate <= @dateTo
                      AND {inner}
                )
            """
        }
    ]

    let getRoutes (startDateBound: LocalDate * LocalDate)
                  (page: int)
                  (searchTerm: string option) () =
        // TODO: Fully async?
        use c = getDbConn ()

        let searchSql, searchParams =
            searchTerm
            |> Option.bind (fun st ->
                st
                |> parseSearch
                |> searchToSql searchFields
                // TODO: Error reporting
                |> Option.ofResult)
            |> Option.defaultValue ("TRUE", ResizeArray())

        let pars = Seq.concat [
            seq { "dateFrom", box <| fst startDateBound
                  "dateTo", box <| snd startDateBound
                  "page", box <| page - 1
                  "perPage", box routesPerPage }
            searchParams
        ]

        let routeCount: int64 =
            sqlQueryOne c $"""
                SELECT COUNT(*)
                FROM routeSummaries AS rs
                WHERE lastDate >= @dateFrom::date
                  AND firstDate <= @dateTo::date
                  AND {searchSql}
                """ pars
            |> unbox

        let routes =
            sqlQueryRec<WebRoute> c $"""
                SELECT routeId, name, firstStop, lastStop
                FROM routeSummaries AS rs
                WHERE lastDate >= @dateFrom::date
                  AND firstDate <= @dateTo::date
                  AND {searchSql}
                -- Order by first numeric component in name
                ORDER BY (regexp_match(name, '\d+'))[1]::int
                LIMIT @perPage
                OFFSET @page * @perPage
                """ pars
            |> Seq.toArray
        routeCount, routes

    let getTrips (startDateBound: LocalDate * LocalDate) (routeId: string) () =
        use c = getDbConn ()
        sqlQueryRec<WebTrip> c """
            SELECT tripId, tripStartDate,
                   COALESCE(shortName, tripId) AS shortName
            FROM tripDetails
            WHERE routeId = @routeId
              AND tripStartDate >= @dateFrom::date
              AND tripStartDate <= @dateTo::date
            """ ["dateFrom", box <| fst startDateBound
                 "dateTo", box <| snd startDateBound
                 "routeId", box routeId]
        |> Seq.toArray

    let routeHtml (r: WebRoute) =
        li [] [
            details [_class "route-heading"
                     attr "data-id" r.routeId] [
                summary [] [
                    span [_class "route-name"] [str r.name]
                    match r.firstStop, r.lastStop with
                    | Some f, Some l ->
                        span [_class "route-stops"]
                             [str (sprintf "(%s -> %s)" f l)]
                    | _ -> empty
                ]
            ]
        ]

    let tripDaysHtml (trips: WebTrip array) =
        ul [_class "days-list"] [
            for t in trips ->
            let link = Links.trip t.tripId t.tripStartDate
            let caption = sprintf "On %s" (dateToIso t.tripStartDate)
            li [] [a [_href link] [str caption]]
        ]

    let tripsHtml fromDate toDate (trips: WebTrip array) =
        ul [_class "trips-list"] [
            for tripId, ts in trips |> Array.groupBy (fun t -> t.tripId) ->
            let firstTrip = ts |> Array.head
            let caption =
                sprintf "%s" firstTrip.shortName
            let link = Links.trips tripId fromDate toDate

            li [] [
                details [] [
                    summary [] [
                        a [_class "trips-heading"
                           _href link]
                          [str caption]
                        span [_class "trips-expand"] [str "Expand"]
                    ]
                    tripDaysHtml ts
                ]
            ]
        ]

    [<HttpGet>]
    member this.get(
            fromDate: LocalDate,
            toDate: LocalDate,
            search: string,
            page: int) =
        let fromDate, toDate = dateRangeOrDefaults fromDate toDate
        let page = if page = 0 then 1 else page
        let routeCount, routes =
            getRoutes (fromDate, toDate) page (nullToNone search) ()

        div [_class "routes-page"] [
            form [_class "controls"; _method "GET"] [
                dateRangeControl fromDate toDate 
                div [_class "search"] [
                    input [_placeholder "Search term"
                           _name "search"
                           _value search]
                    button [] [str "Search"]
                ]
            ]
            h2 [] [str "Routes"]
            ul [_class "route-list"] [
                for route in routes -> routeHtml route ]
            div [_class "pagination"] [
                let pageCount =
                    (float routeCount) / (float routesPerPage)
                    |> ceil |> int
                let linkToPage p =
                    Links.routes fromDate toDate search p
                if page > 1 then
                    yield a
                        [_class "back"
                         _href <| linkToPage (page - 1)]
                        [str "Back"]
                yield span [] [
                    str <| if pageCount = 0 then "No routes"
                           else $"Page {page}/{pageCount}"]
                if page < pageCount then
                    yield a
                        [_class "forward"
                         _href <| linkToPage (page + 1)]
                        [str "Forward"]
            ]
        ]
        |> htmlResult "Route list"

    [<HttpGet("TripsList")>]
    member this.getTripsList(
            fromDate: LocalDate,
            toDate: LocalDate,
            tripId: string) =
        let fromDate, toDate = dateRangeOrDefaults fromDate toDate
        
        tripsHtml fromDate toDate (getTrips (fromDate, toDate) tripId ())
        |> htmlFragmentResult
