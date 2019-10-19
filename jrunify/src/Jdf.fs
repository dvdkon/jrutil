// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Jdf

open System.IO
open System.Data.Common
open Npgsql

open JrUtil
open JrUtil.GtfsMerge
open JrUtil.SqlRecordStore

open JrUnify.Utils

let loadCisCoords (conn: NpgsqlConnection) pathOpt =
    cleanAndSetSchema conn "cis_coords"
    executeSql conn """
        CREATE TABLE stop_coords (
            stop_name text PRIMARY KEY,
            lat numeric, lon numeric);
        """ []
    // Leave an empty table if pathOpt is None
    pathOpt |> Option.iter (fun path ->
        use writer =
            conn.BeginTextImport("COPY stop_coords FROM STDIN (FORMAT CSV)")
        writer.Write(File.ReadAllText(path)))

let applyCisCoords (conn: NpgsqlConnection) gtfsSchema =
    setSchema conn (gtfsSchema + ",cis_coords")
    executeSql conn """
        UPDATE stops AS s
        SET lat = c.lat, lon = c.lon
        FROM stop_coords AS c
        WHERE c.stop_name = s.name
        """ []

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
    if (zeroAgencies
        |> Array.length) > 1 then
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
    fun (conn: DbConnection) stopIdsCis inPath ->
        cleanGtfsTables conn
        let jdf = jdfParser inPath
        let jdf = fixupJdf jdf
        let gtfs = JdfToGtfs.getGtfsFeed stopIdsCis jdf
        Gtfs.sqlInsertGtfsFeed conn gtfs
        ()

let processJdf conn stopIdsCis group path =
    let schemaMerged = sprintf "%s_merged" group
    let schemaTemp = sprintf "%s_temp" group
    let schemaIntermediate = sprintf "%s_intermediate" group

    cleanAndSetSchema conn schemaTemp
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new MergedFeed(conn, schemaMerged, TripMergeStrategy.Never, false)
    cleanAndSetSchema conn schemaIntermediate
    Gtfs.sqlCreateGtfsTables conn

    Directory.EnumerateDirectories(path)
    |> Seq.iter (fun jdfPath ->
        printfn "Processing %s: %s" group jdfPath
        try
            setSchema conn schemaIntermediate
            jdfToGtfsDb conn stopIdsCis jdfPath
            setSchema conn schemaTemp
            mergedFeed.InsertFeed schemaIntermediate
        with
        | :? PostgresException as e ->
            printfn "Error while processing %s:\nSQL error at %s:%s:%d:\n%s"
                    jdfPath e.File e.Line e.Position e.Message
        | e ->
            printfn "Error while processing %s:\n%A" jdfPath e
    )

    applyCisCoords conn schemaMerged
