// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module GeoReport.Processing

open System
open System.IO

open FSharp.Data
open NetTopologySuite.Geometries

open JrUtil.GeoData.Common
open JrUtil.GeoData.Osm
open JrUtil.GeoData.ExternalCsv
open JrUtil.GeoData.StopMatcher
open JrUtil.Utils

let similarityThreshold = 0.8

type MatchByName = {
    stopName: string
    score: float32
}

type StopMatchType =
    | OsmByTag of osmId: int64
    | OsmByName of osmId: int64 * MatchByName
    | ExternalById of source: string
    | ExternalByName of source: string * MatchByName

type StopMatch = {
    matchType: StopMatchType
    point: Point
}

type Sr70Stops = CsvProvider<HasHeaders = false, Schema = "sr70(string), name(string)">

let readStopNamesCsv path =
    File.ReadAllLines(path)
    |> Array.map (fun line ->
        if ((Seq.head line) = '"' && (Seq.last line) = '"')
        then line.[1..(line.Length - 2)]
        else line)

let matchPriority = function
    | OsmByTag _ -> 1
    | OsmByName _ -> 2
    | ExternalById _ -> 3
    | ExternalByName _ -> 4

let topMatches matches =
    matches
    |> Seq.groupBy (fun m -> matchPriority m.matchType)
    |> Seq.sortBy (fun (prio, _) -> prio)
    |> Seq.tryHead
    |> Option.map snd

let getRailStopsMatches czPbf stopsPath extSourcesDir =
    let stops =
        Sr70Stops.Load(
            Path.Combine(Environment.CurrentDirectory, stopsPath)).Rows

    let stopsOsm =
        getCzRailStops czPbf
        |> Seq.map (fun s -> {
            name = s.name |> Option.defaultValue ""
            data = {|
                sr70 = s.sr70
                point = s.point |> pointWgs84ToEtrs89Ex
                source = "OSM"
                osmId = Some <| s.id
            |}
        })
        |> Seq.toArray
    let externalData =
        Directory.EnumerateFiles(extSourcesDir)
        |> Seq.collect (fun path ->
            let sourceName = Path.GetFileNameWithoutExtension(path)
            CzRailStops.Load(Path.GetFullPath(path)).Rows
            |> Seq.map (fun r -> {
                name = r.Name
                data = {|
                    sr70 = Some <| normaliseSr70 r.Sr70
                    point = wgs84Factory.CreatePoint(Coordinate(r.Lon, r.Lat))
                          |> pointWgs84ToEtrs89Ex
                    source = sourceName
                    osmId = None
                |}
            }))
        |> Seq.toArray

    let stopsToMatch =
        Array.concat [ stopsOsm; externalData ]
    let stopsBySr70 =
        stopsToMatch
        |> Array.choose (fun s -> s.data.sr70 |> Option.map (fun k -> k, s))
        |> Array.groupBy fst
        |> Map

    use matcher = new StopMatcher<_>(stopsToMatch)
    stops
    |> Seq.map (fun stop ->
        let nameMatches =
            matcher.matchStop(stop.Name)
            |> Array.map (fun m ->
                let d = m.stop.data
                let matchDesc = {
                    stopName = m.stop.name
                    score = m.score
                }
                {
                    matchType =
                        match d.osmId with
                        | Some i -> OsmByName (i, matchDesc)
                        | None -> ExternalByName (d.source, matchDesc)
                    point = d.point
                })
        let sr70Matches =
            stopsBySr70
            |> Map.tryFind (normaliseSr70 stop.Sr70)
            |> Option.defaultValue [||]
            |> Array.map (fun s ->
                let _, matchedStop = s
                let d = matchedStop.data
                let matchDesc = {
                    stopName = matchedStop.name
                    score = 1000f
                }
                {
                    matchType =
                        match d.osmId with
                        | Some i -> OsmByTag i
                        | None -> ExternalById d.source
                    point = d.point
                })
        sprintf "[%s] %s" stop.Sr70 stop.Name,
        Array.concat [ nameMatches; sr70Matches ])
    |> Seq.toArray

let getOtherStopsMatches czPbf stopsPath extSourcesDir =
    let stops = readStopNamesCsv stopsPath

    let stopsOsm =
        getCzOtherStops czPbf
        |> Seq.map (fun s -> {
            name = czOtherStopNameRegion s |> fst
            data = {|
                point = s.point |> pointWgs84ToEtrs89Ex
                source = "OSM"
                osmId = Some <| s.id
            |}
        })
        |> Seq.toArray
    let externalData =
        Directory.EnumerateFiles(extSourcesDir)
        |> Seq.collect (fun path ->
            let sourceName = Path.GetFileNameWithoutExtension(path)
            OtherStops.Load(Path.GetFullPath(path)).Rows
            |> Seq.map (fun r -> {
                name = r.Name
                data = {|
                    point = wgs84Factory.CreatePoint(Coordinate(r.Lon, r.Lat))
                          |> pointWgs84ToEtrs89Ex
                    source = sourceName
                    osmId = None
                |}
            }))
        |> Seq.toArray

    let stopsToMatch =
        Array.concat [ stopsOsm; externalData ]

    use matcher = new StopMatcher<_>(stopsToMatch)
    stops
    |> Seq.map (fun stop ->
        let matches =
            matcher.matchStop(stop)
            |> Array.map (fun m ->
                let d = m.stop.data
                let matchDesc = {
                    stopName = m.stop.name
                    score = m.score
                }
                {
                    matchType =
                        match d.osmId with
                        | Some i -> OsmByName (i, matchDesc)
                        | None -> ExternalByName (d.source, matchDesc)
                    point = d.point
                })
        stop, matches)
    |> Seq.toArray
