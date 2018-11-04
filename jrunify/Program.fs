// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open Docopt

open System
open System.IO
open System.Data.Common
open Npgsql

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.GtfsModelMeta

let docstring = (fun (s: string) -> s.Trim()) """
jrunify, a tool for combining czech public transport data into a single
GTFS feed

Usage: jrunify.exe <connstr> <jdf-bus> <jdf-mhd> <czptt-szdc> <out>
"""

let setSchema conn schemaName =
    executeSql conn (sprintf "SET search_path TO %s" schemaName) []

let cleanAndSetSchema conn schemaName =
    let sql = 
        sprintf """
                DROP SCHEMA IF EXISTS %s CASCADE;
                CREATE SCHEMA IF NOT EXISTS %s;
                SET search_path TO %s;
                """
                schemaName schemaName schemaName
    executeSql conn sql []

let fixupJdf (jdfBatch: JdfModel.JdfBatch) =
    // Some JDF files (only v1.9?) have multiple agencies with IČO set to 0
    // (because they're not Czech and therefore don't have one). As it's used
    // as a primary key, this causes quite a lot of trouble down the road.
    // As it's impossible to distinguish between multiple rows with the same
    // primary key, this script takes all such agencies and combines them into
    // one "composite agency" with IČO 0, then deletes the rest.

    let zeroAgencies = jdfBatch.agencies |> Array.filter (fun a -> a.id = 0)
    // This process only applies to cases where the agencies aren't
    // distinguished further by "idDistinction"
    if (zeroAgencies
        |> Array.groupBy (fun a -> a.idDistinction)
        |> Array.length) = 1 then
        let names = zeroAgencies |> Array.map (fun a -> a.name)
        let addresses = zeroAgencies |> Array.map (fun a -> a.officeAddress)
        let phoneNums = zeroAgencies |> Array.map (fun a -> a.officePhoneNum)
        let composite: JdfModel.Agency = {
            id = 0
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
            |> Array.filter (fun a -> a.id <> 0)
            |> Array.append [| composite |]
        { jdfBatch with
            agencies = agencies
        }
    else jdfBatch

let cleanGtfsTables conn =
    let cleanSql = """
    DELETE FROM agencies;
    DELETE FROM stops;
    DELETE FROM routes;
    DELETE FROM trips;
    DELETE FROM stopTimes;
    DELETE FROM calendar;
    DELETE FROM calendarExceptions;
    """
    executeSql conn cleanSql []

let jdfToGtfsDb =
    let jdfParser = Jdf.jdfBatchDirParser ()
    fun (conn: DbConnection) inPath ->
        cleanGtfsTables conn
        let jdf = jdfParser inPath
        let jdf = fixupJdf jdf
        let gtfs = JdfToGtfs.getGtfsFeed jdf
        Gtfs.sqlInsertGtfsFeed conn gtfs
        ()

let processJdf conn group path =
    let schemaMerged = sprintf "%s_merged" group
    let schemaTemp = sprintf "%s_temp" group
    let schemaIntermediate = sprintf "%s_intermediate" group
    cleanAndSetSchema conn schemaTemp
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new GtfsMerge.MergedFeed(conn, schemaMerged, false, false)
    cleanAndSetSchema conn schemaIntermediate
    Gtfs.sqlCreateGtfsTables conn
    Directory.EnumerateDirectories(path)
    |> Seq.iter (fun jdfPath ->
        printfn "Processing %s: %s" group jdfPath
        try
            setSchema conn schemaIntermediate
            jdfToGtfsDb conn jdfPath
            setSchema conn schemaTemp
            mergedFeed.InsertFeed schemaIntermediate
        with
        | :? PostgresException as e ->
            printfn "Error while processing %s:\nSQL error at %s:%s:%d:\n%s"
                    jdfPath e.File e.Line e.Position e.Message
        | e ->
            printfn "Error while processing %s:\n%A" jdfPath e
    )

let processCzPtt conn path =
    let schemaMerged = "czptt_merged"
    let schemaTemp = "czptt_temp"
    let schemaIntermediate = "czptt_intermediate"
    cleanAndSetSchema conn schemaTemp
    cleanAndSetSchema conn schemaMerged
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new GtfsMerge.MergedFeed(conn, schemaMerged, false, true)
    cleanAndSetSchema conn schemaIntermediate
    Gtfs.sqlCreateGtfsTables conn
    Directory.EnumerateFiles(path, "*.xml", SearchOption.AllDirectories)
    |> Seq.iter (fun czpttFile ->
        printfn "Processing CZPTT: %s" czpttFile
        try
            setSchema conn schemaIntermediate
            cleanGtfsTables conn
            let czptt = CzPtt.parseFile czpttFile
            let gtfs = CzPtt.gtfsFeed czptt
            Gtfs.sqlInsertGtfsFeed conn gtfs
            setSchema conn schemaTemp
            mergedFeed.InsertFeed schemaIntermediate
        with
        | :? PostgresException as e ->
            printfn "Error while processing %s:\nSQL error at %s:%s:%d:\n%s\n"
                    czpttFile e.File e.Line e.Position e.Message
            exit 10
        | e ->
            printfn "Error while processing %s:\n%A" czpttFile e
    )

let mergeAll conn =
    cleanAndSetSchema conn "merged"
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new GtfsMerge.MergedFeed(conn, "merged", true, true)
    ["jdfbus_merged"; "jdfmhd_merged"; "czptt_merged"]
    |> List.iter (fun schema ->
        cleanAndSetSchema conn "merge_temp"
        mergedFeed.InsertFeed schema
    )

let saveGtfsSqlTable conn recType table outpath =
    let abspath = Path.GetFullPath(outpath)
    let header = getHeader recType |> String.concat ","
    File.WriteAllText(abspath, header)
    let sql =
        // TODO: SQLi? Parameters don't seem to work
        // Cat trick taken from https://dba.stackexchange.com/a/167076
        // This, of course, makes this program UNIX-specific
        sprintf "COPY %s TO PROGRAM 'cat >> %s' WITH (FORMAT csv)"
                table abspath
    executeSql conn sql []

let saveGtfsSqlSchema conn schema outpath =
    Directory.CreateDirectory(outpath) |> ignore
    let saveTable recType tableName fileName =
        saveGtfsSqlTable
            conn
            recType
            (sprintf "%s.%s" schema tableName)
            (Path.Combine(outpath, fileName))

    saveTable typeof<GtfsModel.Agency> "agencies" "agency.txt"
    saveTable typeof<GtfsModel.Stop> "stops" "stop.txt"
    saveTable typeof<GtfsModel.Route> "routes" "route.txt"
    saveTable typeof<GtfsModel.Trip> "trips" "trip.txt"
    saveTable typeof<GtfsModel.StopTime> "stoptimes" "stop_times.txt"
    saveTable typeof<GtfsModel.CalendarEntry> "calendar" "calendar.txt"
    saveTable typeof<GtfsModel.CalendarException> "calendarexceptions"
                                                  "calendar_dates.txt"

let jrunify dbConnStr jdfBusPath jdfMhdPath czpttSzdcPath outPath =
    let newConn () =
        let c = getPostgresqlConnection dbConnStr
        c.Notice.Add(fun ev ->
            let n = ev.Notice
            if n.Severity <> "NOTICE" then
                printfn "SQL notice: %s" n.MessageText)
        c
    [
        async {
            let c = newConn ()
            c.Open()
            processJdf c "jdfbus" jdfBusPath
            c.Close()
        };
        async {
            let c = newConn ()
            c.Open()
            processJdf c "jdfmhd" jdfMhdPath
            c.Close()
        };
        async {
            let c = newConn ()
            c.Open()
            processCzPtt c czpttSzdcPath
            c.Close()
        };
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    use c = newConn ()
    c.Open()
    mergeAll c
    saveGtfsSqlSchema c "merged" outPath
    c.Close()

[<EntryPoint>]
let main args =
    try
        let docopt = Docopt(docstring)
        let args = docopt.Parse(args)

        jrunify (argValue args.["<connstr>"])
                (argValue args.["<jdf-bus>"])
                (argValue args.["<jdf-mhd>"])
                (argValue args.["<czptt-szdc>"])
                (argValue args.["<out>"])
        0
    with
    | ArgvException(msg) ->
        printfn "%s" msg
        1
