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
    let route =
        (sqlQueryRec<Route> conn "SELECT * FROM routes WHERE id = @id"
                            ["id", routeId]) |> Seq.exactlyOne

    let stops =
        sqlQueryRec<Stop> conn """
            SELECT DISTINCT stops.* FROM trips
            INNER JOIN stoptimes ON stoptimes.tripid = trips.id
            INNER JOIN stops ON stops.id = stoptimes.stopid
            WHERE trips.routeid = @id
            """ ["id", routeId]

    printfn "D2 %A" (stops |> Seq.toList |> List.map (fun s -> s.name))

    let array2DToJagged arr =
        [|for i = 0 to (Array2D.length1 arr) - 1 do
          yield [|for j = 0 to (Array2D.length2 arr) - 1 do
                  if arr.[i, j] <> null
                  then yield arr.[i, j]|]|]

    let stopSeqsByDirection = [
        for direction in [null; "0"; "1"] ->
        sqlQuery conn """
            SELECT i.stopseq, array_agg(i.trip) AS triparr
            FROM
                (SELECT
                    array_agg(st.stopid ORDER BY stopsequence) AS stopseq,
                    array_agg((st.stopid, st.arrivaltime, st.departuretime))
                        AS trip
                FROM stoptimes AS st
                INNER JOIN trips ON st.tripid = trips.id
                WHERE trips.routeid = @id
                    AND trips.directionId IS NOT DISTINCT FROM @direction
                GROUP BY trips.id) AS i
            GROUP BY i.stopseq;
            """ ["id", box routeId; "direction", box direction]
        |> Seq.map (fun row ->
            (row.["stopseq"] :?> string array,
             row.["triparr"] :?> obj[][,]
             |> array2DToJagged
             |> Array.map (Array.map (fun tripStop ->
                 (tripStop.[0] :?> string,
                  tripStop.[1] :?> TimeSpan,
                  tripStop.[2] :?> TimeSpan)
            ))))
    ]

    let lastSome opts =
        opts
        |> Seq.choose id
        |> Seq.tryLast

    let zipWithIndex xs = xs |> Seq.mapi (fun i x -> (i, x))

    let combinedStopSeqs = [|
        for stopSeqs in stopSeqsByDirection ->
            let stopSeqs =
                stopSeqs
                |> Seq.map (fun (ss, _) -> ss)
            printfn "D1 %A" (stopSeqs)
            let maxLen =
                if Seq.length stopSeqs = 0 then 0
                else stopSeqs
                     |> Seq.map Array.length
                     |> Seq.max
            let res =
                [for i = 0 to maxLen do
                    for ssNum, stopSeq in zipWithIndex stopSeqs do
                        if Array.length stopSeq > i then
                            let stop = stopSeq.[i]
                            let mappingToLater =
                                stopSeqs
                                |> Seq.filter (fun ss2 -> ss2 <> stopSeq)
                                |> Seq.mapi (fun ssNum2 ss2 ->
                                    ss2.[i..]
                                    |> Array.mapi (fun i2 stop2 ->
                                        let laterStops =
                                            stopSeq.[i..] |> Set.ofArray
                                        let laterStops2 =
                                            ss2.[i..i+i2] |> Set.ofArray
                                        if stop2 = stop
                                            && (Set.intersect laterStops
                                                              laterStops2
                                                |> Set.count) = 0
                                        then Some (ssNum2, i+i2)
                                        else None)
                                    |> lastSome)
                                |> lastSome
                            yield match mappingToLater with
                                  | None -> ((ssNum, i, ssNum, i),
                                             Some (ssNum, i, stop))
                                  | Some (ssNum2, stopNum) ->
                                      ((ssNum, i, ssNum2, stopNum), None)
            ]
            let stops =
                res
                |> List.map (fun (_, s) -> s)
                |> List.choose id
                |> List.toArray
            let mappings =
                res |> List.map (fun (m, _) -> m)

            let stopIds = stops |> Array.map (fun (_, _, sid) -> sid)

            stopIds, stopSeqs |> Seq.toArray |> Array.mapi (fun ssNum ->
                Array.mapi (fun stopNum stop ->
                    let (_, _, tgtSsNum, tgtStopNum) =
                        mappings
                        |> Seq.find (fun (ss, i, _, _) ->
                            ss = ssNum && i = stopNum)
                    let (tgti, _) =
                        stops
                        |> zipWithIndex
                        |> Seq.find (fun (_, (ss, i, _)) ->
                            ss = tgtSsNum && i = tgtStopNum)
                    tgti
                )
            )
    |]

    combinedStopSeqs
    |> Array.iter (fun (stopIds, mss) ->
        printfn "D3 %A"
                (stopIds |> Array.map (fun stopId ->
                    let stop = stops |> Seq.find (fun s -> s.id = stopId)
                    stop.name
                ))
        printfn "D4 %A" (Seq.map Seq.toList mss |> Seq.toList))
    |> ignore


    stopSeqsByDirection
    |> Seq.iteri (fun directionIndex direction ->
        let _, stopSeqs = combinedStopSeqs.[directionIndex]
        direction |> Seq.iteri (fun stopSeqIndex (_, trips) ->
            let stopSeq = stopSeqs.[stopSeqIndex]
            trips |> Seq.iter (fun trip ->
                printfn "D5 %A" trip
                trip |> Seq.iteri (fun i (_, arr, dep) ->
                    printfn "D6 %d %A %A" stopSeq.[i] arr dep
                )
            )
        )
    )
    |> ignore

    let name =
        route.shortName
        |> Option.orElse route.longName
        |> Option.defaultValue "UNNAMED"
    pageTemplate ("Route – " + name)
    <| div [] [
        h2 [] [str "Timetable"]
        div [] [
            for (dirIndex, (stopIds, stopMaps)) in
                    zipWithIndex combinedStopSeqs ->
            table [] [
                thead [] [ tr [] [
                    //for trip in trips ->
                    //th [] [optStr trip.shortName]
                ] ]
                tbody [] [
                    for i, stopId in zipWithIndex stopIds ->
                    let stop = stops |> Seq.find (fun s -> s.id = stopId)
                    tr [] (List.concat [
                        [th [] [str stop.name]]
                        [for stopSeqIndex, (_, trips)
                              in zipWithIndex stopSeqsByDirection.[dirIndex] do
                         let stopMap = stopMaps.[stopSeqIndex]
                         for tripStops in trips do
                         match
                             (tripStops
                             |> Array.mapi (fun j (_, arr, dep) ->
                                 let mappedStopId = stopMap.[j]
                                 if mappedStopId = i
                                     then Some (td [] [str (sprintf "%A" arr)])
                                     else None)
                             |> Array.choose id) with
                         | [|el|] -> yield el
                         | [||] -> yield td [] []
                         | _ -> failwith "Unreachable"]])
                ]
            ]
        ]
    ]
