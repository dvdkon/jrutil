// This file is part of JrUnify and is licenced under the GNU GPLv3 or later
// (c) 2019 David Koňařík

open System.IO
open Serilog

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore
open JrUtil.GeoData

open JrUnify.Jdf
open JrUnify.CzPtt
open JrUnify.Dpmlj
open JrUnify.MergeAll

let docstring = (fun (s: string) -> s.Trim()) """
jrunify, a tool for combining czech public transport data into a single
GTFS feed

Usage:
    jrunify.exe --connstr=CONNSTR --out=OUT [options]

Options:
    --connstr=CONNSTR     Npgsql connection string
    --out=OUTPATH         Output directory
    --logfile=FILE        Path to log file
    --jdf-bus=PATH        JDF BUS directory (extracted)
    --jdf-mhd=PATH        JDF MHD directory (extracted)
    --czptt-szdc=PATH     CZPTT SŽDC directory (extracted)
    --dpmlj-gtfs=PATH     GTFS from DPMLJ (extracted)
    --overpass-url=URL    OSM Overpass API URL
    --cache-dir=PATH      Overpass result cache directory [default: /tmp]
    --rail-coords=PATH   Directory with CSV external railway stop coords
    --other-coords=PATH  Directory with CSV external non-railway stop coords

This program creates numerous schemas in the given database
"""

let jrunify dbConnStr outPath logFile
            jdfBusPath jdfMhdPath czpttSzdcPath dpmljGtfsPath overpassUrl
            cacheDir railCoordsDir otherCoordsDir=
    setupLogging logFile ()

    Log.Information("Starting JrUnify")

    // Dirty hack to make sure there's no command timeout
    let dbConnStrMod = dbConnStr + ";CommandTimeout=0"
    let newConn () =
        let c = getPostgresqlConnection dbConnStrMod
        c.Notice.Add(fun ev ->
            let n = ev.Notice
            if n.Severity <> "NOTICE" then
                Log.Information("SQL notice: {Message}", n.MessageText))
        c.Open()
        c

    measureTime "Loading CISJR geodata" (fun () ->
        let c = newConn ()
        cleanAndSetSchema c "cisjr_geodata"
        SqlOther.initTables c
        overpassUrl |> Option.iter (fun opu ->
            Osm.getCzOtherStops opu cacheDir |> Osm.otherStopsToSql c)
        otherCoordsDir |> Option.iter (fun rcd ->
            Directory.EnumerateFiles(rcd)
            |> Seq.iter (ExternalCsv.loadOtherStopsToSql c))
        c.Close()
    )

    [
        async {
            jdfBusPath |> Option.iter (fun jdfBusPath ->
                measureTime "Processing jdfbus" (fun () ->
                    let c = newConn ()
                    processJdf c false "jdfbus" jdfBusPath
                    c.Close()
                )
            )
        }
        async {
            jdfMhdPath |> Option.iter (fun jdfMhdPath ->
                measureTime "Processing jdfmhd" (fun () ->
                    let c = newConn ()
                    processJdf c false "jdfmhd" jdfMhdPath
                    c.Close()
                )
            )
        }
        async {
            czpttSzdcPath |> Option.iter (fun czpttSzdcPath ->
                measureTime "Processing czptt" (fun () ->
                    let c = newConn ()
                    processCzPtt c overpassUrl cacheDir
                                 czpttSzdcPath
                    c.Close()
                )
            )
        }
        async {
            dpmljGtfsPath |> Option.iter (fun dpmljGtfsPath ->
                let c = newConn ()
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
    measureTime "Merging" (fun () ->
        mergeAll c
    )
    measureTime "Saving" (fun () ->
        Gtfs.saveGtfsSqlSchema c "merged" outPath
    )
    c.Close()

[<EntryPoint>]
let main args =
    withProcessedArgs docstring args (fun args ->
        jrunify (argValue args "--connstr")
                (argValue args "--out")
                (optArgValue args "--logfile")
                (optArgValue args "--jdf-bus")
                (optArgValue args "--jdf-mhd")
                (optArgValue args "--czptt-szdc")
                (optArgValue args "--dpmlj-gtfs")
                (optArgValue args "--overpass-url")
                (argValue args "--cache-dir")
                (optArgValue args "--rail-coords")
                (optArgValue args "--other-coords")
        0)
