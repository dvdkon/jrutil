// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

module JrUnify.Jdf

open System.IO
open System.Data.Common
open Npgsql
open Serilog

open JrUtil
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

let processJdf conn stopIdsCis group path =
    let schemaMerged = sprintf "%s_merged" group
    let schemaTemp = sprintf "%s_temp" group
    let schemaIntermediate = sprintf "%s_intermediate" group

    cleanAndSetSchema conn schemaTemp
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        MergedFeed(conn, schemaMerged, TripMergeStrategy.Never, false)
    cleanAndSetSchema conn schemaIntermediate
    Gtfs.sqlCreateGtfsTables conn

    Directory.EnumerateDirectories(path)
    |> Seq.iter (fun jdfPath ->
        Log.Information("Converting {Group} {File}", group, jdfPath)
        handleErrors "Converting {Group} {File}" [| group; jdfPath |]
            (fun () ->
                setSchema conn schemaIntermediate
                jdfToGtfsDb conn stopIdsCis jdfPath
                setSchema conn "cisjr_geodata"
                applyOtherStopsGeodata conn schemaIntermediate
                setSchema conn schemaTemp
                mergedFeed.InsertFeed schemaIntermediate
            )
    )
