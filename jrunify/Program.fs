// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

open Docopt

open System.IO
open System.Data.Common
open Npgsql

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.GtfsMerge

let docstring = (fun (s: string) -> s.Trim()) """
jrunify, a tool for combining czech public transport data into a single
GTFS feed

Usage: jrunify.exe --connstr=CONNSTR --out=OUT [options]

Options:
    --connstr=CONNSTR              Npgsql connection string
    --out=OUTPATH                  Output directory
    --jdf-bus=PATH                 JDF BUS directory (extracted)
    --jdf-mhd=PATH                 JDF MHD directory (extracted)
    --czptt-szdc=PATH              CZPTT SŽDC directory (extracted)
    --cis-stop-list=PATH           CIS "zastavky.csv"
    --dpmlj-gtfs=PATH              GTFS from DPMLJ (extracted)

This program creates numerous schemas in the given database
"""

let setSchema conn schemaName =
    executeSql conn (sprintf "SET search_path TO %s, public" schemaName) []

let cleanAndSetSchema conn schemaName =
    let sql =
        sprintf """
                DROP SCHEMA IF EXISTS %s CASCADE;
                CREATE SCHEMA IF NOT EXISTS %s;
                SET search_path TO %s, public;
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

    let zeroAgencies =
        jdfBatch.agencies
        |> Array.filter (fun a -> a.id = 0 && a.idDistinction = 0)
    // This process only applies to cases where the agencies aren't
    // distinguished further by "idDistinction"
    if (zeroAgencies
        |> Array.length) > 1 then
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
    DELETE FROM stopTimes;
    DELETE FROM trips;
    DELETE FROM calendar;
    DELETE FROM calendarExceptions;
    DELETE FROM routes;
    DELETE FROM stops;
    DELETE FROM agencies;
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
        new MergedFeed(conn, schemaMerged, TripMergeStrategy.Never, false)
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
        try
            setSchema conn schemaIntermediate
            cleanGtfsTables conn
            let czptt = CzPtt.parseFile czpttFile
            let gtfs = CzPtt.gtfsFeed czptt
            Gtfs.sqlInsertGtfsFeed conn gtfs
            executeSql conn fixupScript []
            setSchema conn schemaTemp
            mergedFeed.InsertFeed schemaIntermediate
        with
        | :? PostgresException as e ->
            printfn "Error while processing %s:\nSQL error at %s:%s:%d:\n%s\n"
                    czpttFile e.File e.Line e.Position e.Message
        | e ->
            printfn "Error while processing %s:\n%A" czpttFile e
    )

let mergeAll conn =
    cleanAndSetSchema conn "merged"
    Gtfs.sqlCreateGtfsTables conn
    let mergedFeed =
        new MergedFeed(conn, "merged", TripMergeStrategy.WithRoute, true)
    ["dpmlj"; "jdfbus_merged"; "jdfmhd_merged"; "czptt_merged"]
    |> List.iter (fun schema ->
        try
            cleanAndSetSchema conn "merge_temp"
            mergedFeed.InsertFeed schema
        with
        | :? PostgresException as e ->
            printfn "Error while merging %s:\nSQL error at %s:%s:%d:\n%s\n"
                    schema e.File e.Line e.Position e.Message
        | e ->
            printfn "Error while merging %s:\n%A" schema e
    )

let loadCisStopList (conn: NpgsqlConnection) path =
    cleanAndSetSchema conn "cis_stops"
    executeSql conn """
        CREATE TABLE stops (name text PRIMARY KEY);
        --CREATE INDEX ON stops USING GIN (name gin_trgm_ops);
        """ []
    use writer = conn.BeginTextImport("COPY stops FROM STDOUT (FORMAT CSV)")
    writer.Write(File.ReadAllText(path))

let normaliseStopNames conn stopsTable prefix =
    let scriptPath = __SOURCE_DIRECTORY__ + "/NormaliseStopNames.sql"
    let script = File.ReadAllText(scriptPath)
    let template = compileSqlTemplate script
    let sql = template ["tgtstops", stopsTable]
    executeSql conn sql [
        "prefix", box prefix
        "threshold", box 0.7]
    ()

let deduplicateRoutes conn =
    let scriptPath = __SOURCE_DIRECTORY__ + "/DeduplicateRoutes.sql"
    let script = File.ReadAllText(scriptPath)
    executeSql conn script []

let processDpmljGtfs conn path =
    try
        cleanAndSetSchema conn "dpmlj"
        Gtfs.sqlCreateGtfsTablesNoConstrs conn
        Gtfs.sqlLoadGtfsFeed conn path

        // XXX: Workaround. Remove when clean data is available
        executeSql conn """
            DELETE FROM stoptimes
            WHERE NOT EXISTS (SELECT FROM stops WHERE id = stopid)
        """ []

        deduplicateRoutes conn

        executeSql conn """
            UPDATE trips
            SET routeid =
                '-CISR-545'
                || lpad((SELECT shortname FROM routes
                         WHERE id = routeid), 3, '0')
                || '-0';

            UPDATE routes
            SET id = '-CISR-545' || lpad(shortname, 3, '0') || '-0';
        """ []

        Gtfs.sqlCreateGtfsConstrs conn

        // TODO: This sometimes results in wrong stops being merged
        // Maybe getting rid of "cityless" stops from JDF will help?
        // Different prefixes per city will also be needed.
        // That'll probably have to wait for OSM integration
        normaliseStopNames conn "cis_stops.stops" "Liberec"
    with
    | :? PostgresException as e ->
        printfn "Error while processing DPMLJ GTFS:\nSQL error at %s:%s:%d:\n%s\n"
                e.File e.Line e.Position e.Message
    | e ->
        printfn "Error while processing DPMLJ GTFS:\n%A" e

let jrunify dbConnStr outPath
            jdfBusPath jdfMhdPath czpttSzdcPath cisStopList dpmljGtfsPath =
    // Dirty hack to make sure there's no command timeout
    let dbConnStrMod = dbConnStr + ";CommandTimeout=0"
    let newConn () =
        let c = getPostgresqlConnection dbConnStrMod
        c.Notice.Add(fun ev ->
            let n = ev.Notice
            if n.Severity <> "NOTICE" then
                printfn "SQL notice: %s" n.MessageText)
        c
    [
        async {
            jdfBusPath |> Option.iter (fun jdfBusPath ->
                measureTime "Processing jdfbus" (fun () ->
                    let c = newConn ()
                    c.Open()
                    processJdf c "jdfbus" jdfBusPath
                    c.Close()
                )
            )
        };
        async {
            jdfMhdPath |> Option.iter (fun jdfMhdPath ->
                measureTime "Processing jdfmhd" (fun () ->
                    let c = newConn ()
                    c.Open()
                    processJdf c "jdfmhd" jdfMhdPath
                    c.Close()
                )
            )
        };
        async {
            czpttSzdcPath |> Option.iter (fun czpttSzdcPath ->
                measureTime "Processing czptt" (fun () ->
                    let c = newConn ()
                    c.Open()
                    processCzPtt c czpttSzdcPath
                    c.Close()
                )
            )
        };
        async {
            dpmljGtfsPath |> Option.iter (fun dpmljGtfsPath ->
                let c = newConn ()
                c.Open()
                measureTime "Reading CIS stop list" (fun () ->
                    loadCisStopList c (Option.get cisStopList)
                )

                measureTime "Processing DPMLJ GTFS" (fun () ->
                    processDpmljGtfs c dpmljGtfsPath
                )
                c.Close()
            )
        }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    use c = newConn ()
    c.Open()
    measureTime "Merging" (fun () ->
        mergeAll c
    )
    measureTime "Saving" (fun () ->
        Gtfs.saveGtfsSqlSchema c "merged" outPath
    )
    c.Close()

[<EntryPoint>]
let main args =
    try
        let docopt = Docopt(docstring)
        let args = docopt.Parse(args)

        jrunify (argValue args.["--connstr"])
                (argValue args.["--out"])
                (optArgValue args.["--jdf-bus"])
                (optArgValue args.["--jdf-mhd"])
                (optArgValue args.["--czptt-szdc"])
                (optArgValue args.["--cis-stop-list"])
                (optArgValue args.["--dpmlj-gtfs"])
        0
    with
    | ArgvException(msg) ->
        printfn "%s" msg
        1
