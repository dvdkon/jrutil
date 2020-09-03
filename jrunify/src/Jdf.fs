// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Jdf

open System
open System.IO
open System.Threading
open Npgsql
open Serilog
open Serilog.Context

open JrUtil
open JrUtil.Utils
open JrUtil.GtfsMerge
open JrUtil.SqlRecordStore
open JrUtil.GeoData.SqlOther

open JrUnify.Utils

let fixupJdf (jdfBatch: JdfModel.JdfBatch) =
    // Some JDF files (only v1.9?) have multiple agencies with IČO set to 0
    // (because they're not Czech and therefore don't have one). As it's used
    // as a primary key, this causes quite a lot of trouble down the road.
    // As it's impossible to distinguish between multiple rows with the same
    // primary key, this script takes all such agencies and combines them into
    // one "composite agency" with IČO 0, then deletes the rest.

    let zeroAgencies =
        jdfBatch.agencies
        |> Array.filter (fun a -> a.id = "0" && a.idDistinction = 0)
    // This process only applies to cases where the agencies aren't
    // distinguished further by "idDistinction"
    if (zeroAgencies |> Array.length) > 1 then
        let names = zeroAgencies |> Array.map (fun a -> a.name)
        let addresses = zeroAgencies |> Array.map (fun a -> a.officeAddress)
        let phoneNums = zeroAgencies |> Array.map (fun a -> a.officePhoneNum)
        let composite: JdfModel.Agency = {
            id = "0"
            taxId = None
            name = "Composite: " + (String.concat "; " names)
            companyType = JdfModel.Corporation
            personName = ""
            officeAddress = "Composite: " + (String.concat "; " addresses)
            officePhoneNum = "Composite: " + (String.concat "; " phoneNums)
            controlPhoneNum = None
            infoPhoneNum = None
            faxNum = None
            email = None
            website = None
            idDistinction = 0
        }
        let agencies =
            jdfBatch.agencies
            |> Array.filter (fun a -> a.id <> "0")
            |> Array.append [| composite |]
        { jdfBatch with
            agencies = agencies
        }
    else jdfBatch

let jdfToGtfsDb =
    let jdfParser = Jdf.jdfBatchDirParser ()
    fun (conn: NpgsqlConnection) stopIdsCis inPath ->
        cleanGtfsTables conn
        let jdf = jdfParser inPath
        let jdf = fixupJdf jdf
        let gtfs = JdfToGtfs.getGtfsFeed stopIdsCis jdf
        Gtfs.sqlInsertGtfsFeed conn gtfs
        ()

let processJdf conn stopIdsCis group path threadCount =
    let schemaMerged = sprintf "%s_merged" group
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        MergedFeed(conn, schemaMerged, TripMergeStrategy.Never, false)

    // Parsing, loading and converting the JDF is per-thread, merging it into
    // the main merged feed is behind a lock and happens on the main connection
    // TODO: Abstract this parallel iteration with setup
    let dirs = Directory.EnumerateDirectories(path)
    let enumerator = dirs.GetEnumerator()
    let next() =
        lock enumerator (fun () ->
            if enumerator.MoveNext() then Some enumerator.Current else None)
    [for _ in 1..threadCount ->
        async {
            let threadId = Thread.CurrentThread.ManagedThreadId
            let schemaTemp = sprintf "%s_temp_%d" group threadId
            let schemaIntermediate = sprintf "%s_intermediate_%d" group threadId
            // Create a connection per thread
            let threadConn = (conn :> ICloneable).Clone() :?> NpgsqlConnection
            threadConn.Open()
            cleanAndSetSchema threadConn schemaTemp
            cleanAndSetSchema threadConn schemaIntermediate
            Gtfs.sqlCreateGtfsTables threadConn

            let processDir jdfPath =
                use prop = LogContext.PushProperty("InputFile", jdfPath)
                Log.Information("Converting {Group} {File}", group, jdfPath)
                handleErrors "Converting {Group} {File}" [| group; jdfPath |]
                    (fun () ->
                        setSchema threadConn schemaIntermediate
                        jdfToGtfsDb threadConn stopIdsCis jdfPath
                        setSchema threadConn "cisjr_geodata"
                        applyOtherStopsGeodata threadConn schemaIntermediate
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
