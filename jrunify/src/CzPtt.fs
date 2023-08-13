// This file is part of JrUnify and is licenced under the GNU AGPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.CzPtt

open System
open System.IO
open System.Threading
open Npgsql
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.GtfsMerge
open JrUtil.SqlRecordStore
open JrUtil.GeoData
open JrUtil.Utils

open JrUnify.Utils

let processCzPtt conn overpassUrl cacheDir path threadCount =
    // TODO: Filter out non-passenger trains (Sv)
    let fixupScript = File.ReadAllText(__SOURCE_DIRECTORY__ + "/FixupCzptt.sql")

    let schemaMerged = "czptt_merged"
    let geodataSchema = "czptt_geodata"

    measureTime "Loading czptt geodata" (fun () ->
        cleanAndSetSchema conn geodataSchema
        SqlCzptt.initTables conn
        overpassUrl |> Option.iter (fun opu ->
            Osm.getCzRailStops opu cacheDir
            |> Osm.czRailStopsToSql conn)
    )

    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        MergedFeed(conn, schemaMerged, TripMergeStrategy.Never,
                   StopMergeStrategy.ExactName, true)

    let files = Directory.EnumerateFiles(path, "*.xml",
                                         SearchOption.AllDirectories)
    let enumerator = files.GetEnumerator()
    let next() =
        lock enumerator (fun () ->
            if enumerator.MoveNext() then Some enumerator.Current else None)

    [for _ in 1..threadCount ->
        async {
            let threadId = Thread.CurrentThread.ManagedThreadId
            let schemaTemp = sprintf "czptt_temp_%d" threadId
            let schemaIntermediate = sprintf "czptt_intermediate_%d" threadId
            // Create a connection per thread
            let threadConn = (conn :> ICloneable).Clone() :?> NpgsqlConnection
            threadConn.Open()
            cleanAndSetSchema threadConn schemaTemp
            cleanAndSetSchema threadConn schemaIntermediate
            Gtfs.sqlCreateGtfsTables threadConn

            let processDir (czpttFile: string) =
                use prop = LogContext.PushProperty("InputFile", czpttFile)
                Log.Information("Processing czptt {File}", czpttFile)
                handleErrors "Processing czptt {File}" [| czpttFile |] (fun () ->
                    setSchema threadConn schemaIntermediate
                    cleanGtfsTables threadConn
                    let czptt = CzPtt.parseFile czpttFile
                    let gtfs = CzPtt.gtfsFeed czptt
                    Gtfs.sqlInsertGtfsFeed threadConn gtfs
                    executeSql threadConn fixupScript []
                    lock mergedFeed (fun () ->
                        setSchema conn schemaTemp
                        mergedFeed.InsertFeed schemaIntermediate)
                )

            while (match next() with
                   | Some dir ->
                       processDir dir
                       true
                   | None -> false) do ()
        }]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    measureTime "Updating czptt geodata" (fun () ->
        setSchema conn geodataSchema
        SqlCzptt.applyCzpttStopsGeodata conn schemaMerged
    )
