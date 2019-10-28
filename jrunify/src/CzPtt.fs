// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.CzPtt

open System.IO
open Npgsql

open JrUtil
open JrUtil.GtfsMerge
open JrUtil.SqlRecordStore

open JrUnify.Utils

let processCzPtt conn path =
    // TODO: Filter out non-passenger trains (Sv)
    let fixupScript = File.ReadAllText(__SOURCE_DIRECTORY__ + "/FixupCzptt.sql")

    let schemaMerged = "czptt_merged"
    let schemaTemp = "czptt_temp"
    let schemaIntermediate = "czptt_intermediate"

    cleanAndSetSchema conn schemaTemp
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new MergedFeed(conn, schemaMerged, TripMergeStrategy.Never, true)
    cleanAndSetSchema conn schemaIntermediate
    Gtfs.sqlCreateGtfsTables conn

    Directory.EnumerateFiles(path, "*.xml", SearchOption.AllDirectories)
    |> Seq.iter (fun czpttFile ->
        printfn "Processing czptt: %s" czpttFile
        handleErrors (sprintf "processing czptt %s" czpttFile) (fun () ->
            setSchema conn schemaIntermediate
            cleanGtfsTables conn
            let czptt = CzPtt.parseFile czpttFile
            let gtfs = CzPtt.gtfsFeed czptt
            Gtfs.sqlInsertGtfsFeed conn gtfs
            executeSql conn fixupScript []
            setSchema conn schemaTemp
            mergedFeed.InsertFeed schemaIntermediate
        )
    )
