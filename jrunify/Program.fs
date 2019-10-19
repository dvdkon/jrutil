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
    --connstr=CONNSTR              Npgsql connection string
    --out=OUTPATH                  Output directory
    --jdf-bus=PATH                 JDF BUS directory (extracted)
    --jdf-mhd=PATH                 JDF MHD directory (extracted)
    --czptt-szdc=PATH              CZPTT SŽDC directory (extracted)
    --cis-stop-list=PATH           CIS "zastavky.csv"
    --cis-coords=PATH              CSV file with coordinates of CIS stops
    --dpmlj-gtfs=PATH              GTFS from DPMLJ (extracted)

This program creates numerous schemas in the given database
"""

let jrunify dbConnStr outPath
            jdfBusPath jdfMhdPath czpttSzdcPath cisStopList cisCoords
            dpmljGtfsPath =
    // Dirty hack to make sure there's no command timeout
    let dbConnStrMod = dbConnStr + ";CommandTimeout=0"
    let newConn () =
        let c = getPostgresqlConnection dbConnStrMod
        c.Notice.Add(fun ev ->
            let n = ev.Notice
            if n.Severity <> "NOTICE" then
                printfn "SQL notice: %s" n.MessageText)
        c.Open()
        c

    measureTime "Loading CIS coordinates" (fun () ->
        let c = newConn ()
        loadCisCoords c cisCoords
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
                    processCzPtt c czpttSzdcPath
                    c.Close()
                )
            )
        }
        async {
            dpmljGtfsPath |> Option.iter (fun dpmljGtfsPath ->
                let c = newConn ()
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
                (optArgValue args "--jdf-bus")
                (optArgValue args "--jdf-mhd")
                (optArgValue args "--czptt-szdc")
                (optArgValue args "--cis-stop-list")
                (optArgValue args "--cis-coords")
                (optArgValue args "--dpmlj-gtfs")
        0)
