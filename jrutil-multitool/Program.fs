// This file is part of JrUtil` and is licenced under the GNU GPLv3 or later
// (c) 2018 David Koňařík

open System.IO
open Docopt

open JrUtil
open JrUtil.Utils
open JrUtil.SqlRecordStore

let docstring = (fun (s: string) -> s.Trim()) """
jrutil, a tool for working with czech public transport data

Usage:
    jrutil.exe jdf-to-gtfs <JDF-in-dir> <GTFS-out-dir>
    jrutil.exe czptt-to-gtfs <CzPtt-in-file> <GTFS-out-dir>
    jrutil.exe merge-gtfs --db-connstr=CONNSTR [--no-check-stop-type] <GTFS-out-dir> <GTFS-in-dir>...
    jrutil.exe --help

Options
    --db-connstr=CONNSTR
    --no-check-stop-type

Passing - to an input path parameter will make jrutil read input filenames
from stdin. Each result will be output into a sequentially numbered
directory.
"""

// merge_gtfs' argument order being opposite of other commands' is not
// intentional, it's necessary to work around a "bug" in docopt.net

let stdinLinesSeq () =
    Seq.initInfinite (fun i -> stdin.ReadLine())
    |> Seq.takeWhile (fun l -> l <> null)

let inOutFiles inpath outpath =
    if inpath = "-" then
        stdinLinesSeq ()
        |> Seq.mapi (fun i l -> (l, Path.Combine(outpath, string i)))
    else
        seq [(inpath, outpath)]

[<EntryPoint>]
let main (args: string array) =
    try
        let docopt = Docopt(docstring)
        let args = docopt.Parse(args)
        if argFlagSet args.["jdf-to-gtfs"] then
            let jdfPar = Jdf.jdfBatchDirParser ()
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValue args.["<JDF-in-dir>"])
                       (argValue args.["<GTFS-out-dir>"])
            |> Seq.iter (fun (inpath, out) ->
                printfn "Processing %s" inpath
                try
                    let jdf = jdfPar inpath
                    let gtfs = JdfToGtfs.getGtfsFeed jdf
                    gtfsSer out gtfs
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )
        else if argFlagSet args.["czptt-to-gtfs"] then
            let gtfsSer = Gtfs.gtfsFeedToFolder ()
            inOutFiles (argValue args.["<CzPtt-in-file>"])
                       (argValue args.["<GTFS-out-dir>"])
            |> Seq.iter (fun (inpath, out) ->
                printfn "Processing %s" inpath
                try
                    let czptt = CzPtt.parseFile inpath
                    let gtfs = CzPtt.gtfsFeed czptt
                    gtfsSer out gtfs
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )
        else if argFlagSet args.["merge-gtfs"] then
            let dbConnStr = argValue args.["--db-connstr"]
            let checkStopType = not <| argFlagSet args.["--no-check-stop-type"]

            use dbConn = getPostgresqlConnection dbConnStr
            dbConn.Open()

            let infiles = argValues args.["<GTFS-in-dir>"]
            let infiles =
                if infiles |> Seq.tryHead = Some "-"
                then stdinLinesSeq()
                else infiles |> Seq.ofList

            let feedParser = Gtfs.gtfsParseFolder ()

            // TODO!
            (*let mergedFeed = new GtfsMerge.MergedFeed(dbConn, checkStopType)
            Gtfs.sqlCreateGtfsTables dbConn

            // TODO: Async
            infiles
            |> Seq.iter (fun inpath ->
                printfn "Processing %s" inpath
                try
                    let feed = feedParser inpath
                    mergedFeed.InsertFeed feed
                with
                    | e -> printfn "Error while processing %s:\n%A" inpath e
            )

            let feedSerializer = Gtfs.gtfsFeedToFolder ()
            let outPath = argValue args.["<GTFS-out-dir>"]
            feedSerializer outPath (mergedFeed.ToGtfsFeed())*)
            dbConn.Close()
        else printfn "%s" docstring
        0
    with
    | ArgvException(msg) ->
        printfn "%s" msg
        1
