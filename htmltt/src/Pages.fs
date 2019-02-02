// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
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
    ]

let routePage conn routeId =
    let lastSome opts =
        opts
        |> Seq.choose id
        |> Seq.tryLast

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
        let stopsAndMap =
            [for i = 0 to maxStopCount do
                for ssNum, stopSeq in zipWithIndex stopSeqs do
                    if Array.length stopSeq > i then
                        let stop = stopSeq.[i]
                        let mappingToLater =
                            stopSeqs
                            |> zipWithIndex
                            |> Seq.filter (fun (ssNum2, ss2) ->
                                ssNum <> ssNum2
                                && Array.length ss2 >= Array.length stopSeq)
                            |> Seq.map (fun (ssNum2, ss2) ->
                                ss2.[i..]
                                |> Array.mapi (fun i2 stop2 ->
                                    if stop2 = stop then
                                        let laterStops =
                                            stopSeq.[i..] |> Set.ofArray
                                        let laterStops2 =
                                            ss2.[i..i+i2-1] |> Set.ofArray
                                        if (Set.intersect laterStops
                                                          laterStops2
                                            |> Set.count) = 0
                                        then Some (ssNum2, i+i2)
                                        else None
                                    else None)
                                |> lastSome)
                            |> lastSome
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
