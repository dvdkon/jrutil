// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2019 David Koňařík

module HtmlTt.Pages

open System
open System.Web
open Giraffe.GiraffeViewEngine

open JrUtil.GtfsModel
open JrUtil.SqlRecordStore

let urlEncode (str: string) = HttpUtility.UrlEncode(str)

let optStr os = str (os |> Option.defaultValue "")

let linkTr url attrs nodes =
    tr attrs [
        for node in nodes ->
        td [] [a [_href url] [node]]
    ]

// TODO: Proper naming? I can't come up with anything better
let table2 cols content =
    table [] [
        thead [] [
            tr [] [
                for col in cols ->
                th [] [str col]
            ]
        ]
        tbody [] content
    ]

let pageTemplate title_ contents =
    html [] [
        head [] [
            title [] [str title_]
            meta [_charset "UTF-8"]
        ]
        body [] [
            contents
        ]
    ]

let routesTable (routes: Route seq) =
    table2 ["ID"; "Short name"; "Long name"; "Description"; "Type"] [
        for route in routes ->
        linkTr ("/route/" + (urlEncode route.id)) [] [
            str route.id
            optStr route.shortName
            optStr route.longName
            optStr route.description
            str route.routeType
        ]
    ]

let agenciesPage conn =
    let agencies = sqlQueryRec<Agency> conn "SELECT * FROM agencies" []

    pageTemplate "Agencies"
    <| table2 ["ID"; "Name"] [
        for agency in agencies ->
        let aid = agency.id |> Option.defaultValue ""
        linkTr ("/agency/" + (urlEncode aid)) [] [
            str aid
            str agency.name
        ]
    ]

let agencyPage conn agencyId =
    let agency =
        (sqlQueryRec<Agency> conn "SELECT * FROM agencies WHERE id = @id"
                             ["id", agencyId]) |> Seq.exactlyOne
    let routes =
        sqlQueryRec<Route> conn "SELECT * FROM routes WHERE agencyid = @id"
                           ["id", agencyId]

    pageTemplate ("Agency – " + agency.name)
    <| div [] [
        h2 [] [str "Routes"]
        routesTable routes
    ]

let routePage conn routeId =
    let zipWithIndex xs = xs |> Seq.mapi (fun i x -> (i, x))

    let route =
        (sqlQueryRec<Route> conn "SELECT * FROM routes WHERE id = @id"
                            ["id", routeId]) |> Seq.exactlyOne

    let trips =
        sqlQueryRec<Trip> conn "SELECT * FROM trips WHERE routeid = @id"
                          ["id", routeId]

    let stops =
        sqlQueryRec<Stop> conn """
            SELECT DISTINCT stops.* FROM trips
            INNER JOIN stoptimes ON stoptimes.tripid = trips.id
            INNER JOIN stops ON stops.id = stoptimes.stopid
            WHERE trips.routeid = @id
            """ ["id", routeId]

    let byDirection = [|
        for direction in [null; "0"; "1"] do
        // This whole chunk of code tries to create a combined list of stops
        // that would reasonably apply to all trips. This becomes really hard
        // when cycles and branches get involved, so the current algorithm
        // is slow and doesn't always produce pretty or even correct results.
        // The code is also kind of a mess. Sorry.
        let stopSeqsWithTrips =
            sqlQuery conn """
                SELECT i.stopseq, array_agg((i.tripid, i.trip)) AS triparr
                FROM
                    (SELECT
                        array_agg(st.stopid ORDER BY stopsequence) AS stopseq,
                        array_agg((st.stopid, st.arrivaltime, st.departuretime)
                                  ORDER BY stopsequence)
                            AS trip,
                        trips.id AS tripid
                    FROM stoptimes AS st
                    INNER JOIN trips ON st.tripid = trips.id
                    WHERE trips.routeid = @id
                        AND trips.directionId IS NOT DISTINCT FROM @direction
                    GROUP BY trips.id) AS i
                GROUP BY i.stopseq;
                """ ["id", box routeId; "direction", box direction]
            |> Seq.map (fun row ->
                (row.["stopseq"] :?> string array,
                 row.["triparr"] :?> obj[][]
                 |> Array.map (fun trip ->
                     (trip.[0] :?> string,
                      trip.[1] :?> obj[][] |> Array.map (fun tripStop ->
                          (tripStop.[0] :?> string,
                           tripStop.[1] :?> TimeSpan,
                           tripStop.[2] :?> TimeSpan))))))

        let stopSeqs = stopSeqsWithTrips |> Seq.map (fun (ss, _) -> ss)
        let maxStopCount =
            if Seq.length stopSeqs = 0 then 0
            else stopSeqs
                 |> Seq.map Array.length
                 |> Seq.max

        let removeOnce toRemove l =
            let removed, rl = l |> List.fold (fun (shouldRemove, rl) item ->
                if item = toRemove
                then (false, rl)
                else (shouldRemove, item :: rl)) (true, [])
            (not removed, rl)
        let difference l1 l2 =
            l1 |> List.fold (fun l item ->
                let removed, rl = removeOnce item l
                rl) l2

        let firstSome opts =
            opts
            |> Seq.choose id
            |> Seq.tryHead

        let stopsAndMap =
            [for i = 0 to maxStopCount do
                for ssNum, stopSeq in zipWithIndex stopSeqs do
                    if Array.length stopSeq > i then
                        let stop = stopSeq.[i]
                        let mappingToLater =
                            seq {maxStopCount-1 .. -1 .. i}
                            //seq {i .. maxStopCount-1}
                            |> Seq.map (fun i2 ->
                                stopSeqs
                                |> zipWithIndex
                                |> Seq.rev
                                |> Seq.filter (fun (ssNum2, ss2) ->
                                    ssNum <> ssNum2
                                    && (i <> i2 || ssNum2 > ssNum)
                                    && Array.length ss2 > i2)
                                |> Seq.map (fun (ssNum2, ss2) ->
                                    let stop2 = ss2.[i2]
                                    if stop2 = stop then
                                        let prevStops =
                                            if i-1 < 0 then [||]
                                            else stopSeq.[..i-1]
                                        let laterStops =
                                            stopSeq.[i+1..]
                                        let prevStops2 =
                                            if i2-1 < 0 then [||]
                                            else ss2.[..i2-1]
                                        let laterStops2 =
                                            ss2.[i2+1..]

                                        let stc = Array.toList laterStops
                                        let stc2 =
                                            difference
                                                (Seq.toList prevStops2)
                                                (Seq.toList prevStops)
                                        if (Set.intersect (set stc) (set stc2)
                                            |> Set.count) = 0
                                        then Some (ssNum2, i2)
                                        else None
                                    else None)
                                |> firstSome)
                            |> firstSome
                        yield match mappingToLater with
                              | None -> (None, Some (ssNum, i, stop))
                              | Some (ssNum2, stopNum) ->
                                  (Some (ssNum, i, ssNum2, stopNum), None)
        ]
        let stopsWithLoc =
            stopsAndMap
            |> List.map (fun (_, s) -> s)
            |> List.choose id
            |> List.toArray
        let mappings =
            stopsAndMap |> List.map (fun (m, _) -> m) |> List.choose id

        let stopIds = stopsWithLoc |> Array.map (fun (_, _, sid) -> sid)


        let stopMaps =
            stopSeqs |> Seq.toArray |> Array.mapi (fun ssNum ->
                Array.mapi (fun stopNum stop ->
                    let mutable tgtSsNum = ssNum
                    let mutable tgtStopNum = stopNum
                    while mappings |> Seq.exists (fun (ss, i, _, _) ->
                            ss = tgtSsNum && i = tgtStopNum) do
                        let (_, _, newSsNum, newStopNum) =
                            mappings
                            |> Seq.find (fun (ss, i, _, _) ->
                                ss = tgtSsNum && i = tgtStopNum)
                        tgtSsNum <- newSsNum
                        tgtStopNum <- newStopNum
                    let (tgti, _) =
                        stopsWithLoc
                        |> zipWithIndex
                        |> Seq.find (fun (_, (ss, i, _)) ->
                            ss = tgtSsNum && i = tgtStopNum)
                    tgti
                )
            )

        let stopTimesTable = [|
            for stopSeqIndex, (_, trips)
                in zipWithIndex stopSeqsWithTrips do
            let stopMap = stopMaps.[stopSeqIndex]
            for (tripId, tripStops) in trips ->
            [|
                for stopIndex = 0 to Seq.length stopIds do
                yield
                    stopMap
                    |> Seq.tryFindIndex ((=) stopIndex)
                    |> Option.map (fun n -> tripStops.[n])
            |]
        |]

        let tableTrips = [
            for (_, tripIds) in stopSeqsWithTrips do
            for (tripId, _) in tripIds do
            yield trips |> Seq.find (fun t -> t.id = tripId)
        ]

        let tableStops =
            stopIds
            |> Array.map (fun sid -> stops |> Seq.find (fun s -> s.id = sid))

        if Seq.length tableTrips > 0 then
            yield (stopTimesTable, tableTrips, tableStops)
    |]

    let stopTimeToTd = function
        | Some (_, time: TimeSpan, _) ->
            td [] [str <| sprintf "%02d:%02d" time.Hours time.Minutes]
        | None -> td [] []

    let routeName =
        route.shortName
        |> Option.orElse route.longName
        |> Option.defaultValue "UNNAMED"

    pageTemplate ("Route – " + routeName)
    <| div [] [
        h2 [] [str "Timetable"]
        div [] [
            for timeTable, tableTrips, stops in byDirection ->
            table [] [
                thead [] [
                    tr [] (
                        [th [] []] @
                        [for trip in tableTrips ->
                            th [] [optStr trip.shortName]
                    ])
                ]
                tbody [] [
                    for stopIndex, stop in zipWithIndex stops ->
                    tr [] (
                        [th [] [str stop.name]] @
                        [for trip in timeTable ->
                            stopTimeToTd trip.[stopIndex]])]
            ]
        ]
    ]

let stopsPage conn =
    let stops = sqlQueryRec<Stop> conn "SELECT * FROM stops" []

    pageTemplate "Stops"
    <| div [] [
        h1 [] [str "Stops"]
        table2 ["ID"; "Code"; "Lat"; "Lon"; "Name"; "Type"; "Platform"] [
            for stop in stops ->
            linkTr ("/stop/" + (urlEncode stop.id)) [] [
                str stop.id
                optStr stop.code
                optStr (stop.lat |> Option.map string)
                optStr (stop.lon |> Option.map string)
                str stop.name
                optStr (stop.locationType |> Option.map string)
                optStr stop.platformCode
            ]
        ]
    ]

let stopPage conn stopId =
    let stop =
        sqlQueryRec<Stop> conn "SELECT * FROM stops WHERE id = @id"
                          ["id", stopId] |> Seq.exactlyOne

    let routes =
        sqlQueryRec<Route> conn """
            SELECT routes.* FROM routes
            WHERE EXISTS (
                SELECT FROM trips
                INNER JOIN stoptimes ON stoptimes.tripid = trips.id
                WHERE trips.routeid = routes.id AND stoptimes.stopid = @stopid)
        """ ["stopid", stopId]

    pageTemplate ("Stop – " + stop.name)
    <| div [] [
        h2 [] [str "Routes"]
        routesTable routes
    ]
