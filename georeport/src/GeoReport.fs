// This file is part of JrUtil and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module GeoReport.Processing

open System
open System.IO

open FSharp.Data

open JrUtil.SqlRecordStore
open JrUtil.GeoData.Osm
open JrUtil.GeoData.ExternalCsv

let similarityThreshold = 0.8

type StopMatchType =
    | OsmMatchByTag of int64
    | OsmMatchByName of int64
    | ExternalSource of string


type StopMatch = {
    matchType: StopMatchType
    lat: float
    lon: float
}

type Sr70Stops = CsvProvider<HasHeaders = false, Schema = "sr70(int), name(string)">

let readStopNamesCsv path =
    File.ReadAllLines(path)
    |> Array.map (fun line ->
        if ((Seq.head line) = '"' && (Seq.last line) = '"')
        then line.[1..(line.Length - 2)]
        else line)

let topMatches matches =
    let matchPriority = function
        | OsmMatchByTag _ -> 1
        | OsmMatchByName _ -> 2
        | ExternalSource _ -> 3

    matches
    |> Seq.groupBy (fun m -> matchPriority m.matchType)
    |> Seq.sortBy (fun (prio, _) -> prio)
    |> Seq.tryHead
    |> Option.map snd

let initSqlTables conn =
    executeSql conn """
        CREATE TABLE otherstops (
            name text NOT NULL
        );
        CREATE INDEX ON otherstops USING GIST (name gist_trgm_ops);

        CREATE TABLE railstops (
            LIKE otherstops,
            sr70 text NOT NULL
        );
        CREATE INDEX ON railstops USING GIST (name gist_trgm_ops);

        CREATE TABLE otherstops_external (
            LIKE otherstops,
            source text NOT NULL,
            lat float NOT NULL,
            lon float NOT NULL
        );
        CREATE INDEX ON otherstops_external USING GIST (name gist_trgm_ops);

        CREATE TABLE railstops_external (
            LIKE otherstops_external,
            sr70 text
        );
        CREATE INDEX ON railstops_external USING GIST (name gist_trgm_ops);

        CREATE TABLE otherstops_osm (
            LIKE otherstops,
            osmid bigint PRIMARY KEY,
            lat float NOT NULL,
            lon float NOT NULL
        );
        CREATE INDEX ON otherstops_osm USING GIST (name gist_trgm_ops);

        CREATE TABLE railstops_osm (
            LIKE otherstops_osm,
            sr70 text
        );
        CREATE INDEX ON railstops_osm USING GIST (name gist_trgm_ops);
    """ []

    // XXX: Hope this sticks
    executeSql
        conn
        (sprintf "SET pg_trgm.similarity_threshold = %f"
                 similarityThreshold) []

let loadStopListsToSql conn railStopsPath otherStopsPath =
    let railStops =
        Sr70Stops.Load(
            Path.Combine(Environment.CurrentDirectory, railStopsPath))
    // TODO: Does this include tram/metro stops?
    let otherStops = readStopNamesCsv otherStopsPath

    sqlCopyInText conn "railstops" [| false; false |]
        (railStops.Rows
         |> Seq.map (fun rs -> [|
             rs.Name
             (let sr70 = string rs.Sr70
              if sr70.Length > 5 then sr70.[..4] else sr70)|]))

    sqlCopyInText conn "otherstops" [| false |]
        (otherStops
         |> Seq.map (fun name -> [| name |]))

let loadOsmDataToSql conn overpassUrl cacheDir =
    let railStopsOsm = getCzRailStops overpassUrl cacheDir
    let otherStopsOsm = getCzOtherStops overpassUrl cacheDir

    sqlCopyInText conn "railstops_osm" [| false; false; false; false; false |]
        (railStopsOsm
         |> Seq.map (fun rso -> [|
            czRailStopName rso
            string rso.Id
            string rso.Lat
            string rso.Lon
            normalisedSr70 rso
         |]))

    sqlCopyInText conn "otherstops_osm" [| false; false; false; false |]
        (otherStopsOsm
         |> Seq.map (fun oso -> [|
             oso.Tags.Name
             string oso.Id
             string oso.Lat
             string oso.Lon
         |]))

let loadExternalDataToSql conn railExtSourcesDir otherExtSourcesDir =
    Directory.EnumerateFiles(railExtSourcesDir)
    |> Seq.iter (fun path ->
            sqlCopyInText
                conn "railstops_external" [| false; false; false; false; true |]
                (CzRailStops.Load(Path.GetFullPath(path)).Rows
                 |> Seq.map (fun s -> [|
                     s.Name
                     Path.GetFileNameWithoutExtension(path)
                     string s.Lat
                     string s.Lon
                     string s.Sr70
                 |])))

    Directory.EnumerateFiles(otherExtSourcesDir)
    |> Seq.iter (fun path ->
            sqlCopyInText
                conn "otherstops_external" [| false; false; false; false |]
                (OtherStops.Load(Path.GetFullPath(path)).Rows
                 |> Seq.map (fun s -> [|
                     s.Name
                     Path.GetFileNameWithoutExtension(path)
                     string s.Lat
                     string s.Lon
                 |])))

let rowToMatch typeGetter =
    Seq.map (fun (res: SqlRow) ->
        (res.["name"] :?> string),
        {
            matchType = typeGetter res
            lat = (unbox res.["lat"])
            lon = (unbox res.["lon"])
        })

let rowToOsmMatch constructor =
    rowToMatch (fun r -> constructor (unbox r.["osmid"]))

let rowToExternalMatch =
    rowToMatch (fun r -> ExternalSource (unbox r.["source"]))

let getRailStopsMatches conn =
    // Sorry for all the copy&paste, I feel using just one query would be
    // too unclear and concatting SQL strings is not even an option in that
    // regard
    let railStopMatches =
        Seq.concat [
            sqlQuery conn """
                SELECT rs.name, osmid, lat, lon
                FROM railstops AS rs
                INNER JOIN railstops_osm AS rso
                    ON rs.sr70 = rso.sr70
            """ []
            |> rowToOsmMatch OsmMatchByTag

            sqlQuery conn """
                SELECT rs.name, osmid, lat, lon
                FROM railstops AS rs
                INNER JOIN railstops_osm AS rso
                    ON rs.name % rso.name
                ORDER BY rs.name <-> rso.name
            """ []
            |> rowToOsmMatch OsmMatchByName

            sqlQuery conn """
                SELECT rs.name, source, lat, lon
                FROM railstops AS rs
                INNER JOIN railstops_external AS rse
                    ON rs.sr70 = rse.sr70
                UNION
                -- ORDER BY doesn't work on one query in a UNION
                SELECT * FROM (
                    SELECT rs.name, source, lat, lon
                    FROM railstops AS rs
                    INNER JOIN railstops_external AS rse
                        ON rs.name % rse.name
                    ORDER BY rs.name <-> rse.name
                ) AS i
            """ []
            |> rowToExternalMatch
        ]
        |> Seq.groupBy (fun (n, m) -> n)
        |> Seq.map (fun (n, ms) -> n, ms |> Seq.map (fun (_, m) -> m))

    let railStopsNoMatch =
        sqlQuery conn """
        SELECT name
        FROM railstops
        WHERE name <> ALL(@names)
        """ ["names", box (railStopMatches
                           |> Seq.map (fun (n, _) -> n)
                           |> Seq.toArray)]
        |> Seq.map (fun r -> r.[0] :?> string, seq [])

    Seq.concat [railStopMatches; railStopsNoMatch]

let getOtherStopsMatches conn =
    let otherStopMatches =
        Seq.concat [
            sqlQuery conn """
                SELECT os.name, osmid, lat, lon
                FROM otherstops AS os
                INNER JOIN otherstops_osm AS oso
                    ON os.name % oso.name
                ORDER BY os.name <-> oso.name
            """ []
            |> rowToOsmMatch OsmMatchByName

            sqlQuery conn """
                SELECT os.name, source, lat, lon
                FROM otherstops AS os
                INNER JOIN otherstops_external AS ose
                    ON os.name % ose.name
                ORDER BY os.name <-> ose.name
            """ []
            |> rowToExternalMatch
        ]
        |> Seq.groupBy (fun (n, m) -> n)
        |> Seq.map (fun (n, ms) -> n, ms |> Seq.map (fun (_, m) -> m))

    let otherStopsNoMatch =
        sqlQuery conn """
        SELECT name
        FROM otherstops
        WHERE name <> ALL(@names)
        """ ["names", box (otherStopMatches
                           |> Seq.map (fun (n, _) -> n)
                           |> Seq.toArray)]
        |> Seq.map (fun r -> r.[0] :?> string, seq [])

    Seq.concat [otherStopMatches; otherStopsNoMatch]
